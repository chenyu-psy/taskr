# Local Dashboard Control Server (Internal)
#
# Purpose:
# - Run a localhost-only HTTP control server in the main R console.
# - Let the external dashboard process request cancel/clear operations against
#   the live scheduler and live task process handles.

new_control_token <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%OS6"),
    "_",
    Sys.getpid(),
    "_",
    paste0(sample(c(letters, LETTERS, 0:9), size = 24, replace = TRUE), collapse = "")
  )
}

control_server_is_running <- function() {
  srv <- pkg_env$control_server %||% NULL
  !is.null(srv) && is.function(srv$isRunning) && isTRUE(srv$isRunning())
}

control_server_url <- function() {
  url <- pkg_env$control_url %||% ""
  if (!is.character(url) || length(url) != 1 || is.na(url)) {
    return("")
  }
  url
}

control_server_token <- function() {
  token <- pkg_env$control_token %||% ""
  if (!is.character(token) || length(token) != 1 || is.na(token) || !nzchar(token)) {
    pkg_env$control_token <- new_control_token()
    token <- pkg_env$control_token
  }
  token
}

control_service_loop_is_running <- function() {
  is.function(pkg_env$control_service_handle %||% NULL)
}

schedule_control_service_loop <- function(delay = 0.05) {
  if (!control_server_is_running()) {
    return(invisible(FALSE))
  }
  if (control_service_loop_is_running()) {
    return(invisible(FALSE))
  }

  pkg_env$control_service_handle <- later::later(
    func = control_service_tick,
    delay = as.numeric(delay)
  )
  invisible(TRUE)
}

control_service_tick <- function() {
  pkg_env$control_service_handle <- NULL
  if (!control_server_is_running()) {
    return(invisible(NULL))
  }

  # A small positive timeout lets httpuv process pending localhost requests
  # without blocking the main console or starving scheduler ticks.
  try(httpuv::service(1), silent = TRUE)
  schedule_control_service_loop()
  invisible(NULL)
}

stop_control_service_loop <- function() {
  handle <- pkg_env$control_service_handle %||% NULL
  if (is.function(handle)) {
    try(handle(), silent = TRUE)
  }
  pkg_env$control_service_handle <- NULL
  invisible(NULL)
}

control_json_response <- function(status = 200L, payload = list()) {
  list(
    status = as.integer(status),
    headers = list(
      "Content-Type" = "application/json",
      "Access-Control-Allow-Origin" = "*",
      "Access-Control-Allow-Methods" = "GET, POST, OPTIONS",
      "Access-Control-Allow-Headers" = "Content-Type"
    ),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  )
}

control_options_response <- function() {
  list(
    status = 204L,
    headers = list(
      "Access-Control-Allow-Origin" = "*",
      "Access-Control-Allow-Methods" = "GET, POST, OPTIONS",
      "Access-Control-Allow-Headers" = "Content-Type"
    ),
    body = ""
  )
}

read_control_request_body <- function(req) {
  input <- req$rook.input %||% NULL
  if (is.null(input) || is.null(input$read) || !is.function(input$read)) {
    return("")
  }

  chunks <- character()
  repeat {
    chunk <- input$read()
    if (length(chunk) == 0 || is.null(chunk)) {
      break
    }
    if (is.raw(chunk)) {
      chunk <- rawToChar(chunk)
    }
    chunks <- c(chunks, as.character(chunk))
  }

  paste0(chunks, collapse = "")
}

parse_control_json_body <- function(req) {
  body <- read_control_request_body(req)
  if (!nzchar(body)) {
    return(list())
  }

  out <- tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(out)) {
    stop("Request body must be valid JSON.")
  }
  as.list(out)
}

validate_control_token <- function(payload) {
  got <- as.character(payload$token %||% "")
  expected <- control_server_token()
  if (!identical(got, expected)) {
    stop("Invalid dashboard control token.")
  }
  invisible(TRUE)
}

control_cancel_task <- function(task_id) {
  task_id <- normalize_task_id(task_id, name = "task_id")
  cancel_task(task_id)
  write_dashboard_snapshot()
  list(
    ok = TRUE,
    action = "cancel",
    task_id = task_id,
    status = "cancelled",
    message = "Task cancellation completed."
  )
}

control_remove_task <- function(task_id) {
# Remove one task through the dashboard control server.
#
# Purpose:
# - Let the background dashboard remove one task without clearing the whole
#   queue or all finished records.
#
# Parameters:
# - `task_id`: Task id to remove.
#
# Returns:
# - List payload for the dashboard control response.
#
# Assumptions and side effects:
# - `remove_task()` cancels pending/running tasks before removing their records.
  task_id <- normalize_task_id(task_id, name = "task_id")
  remove_task(task_id)
  write_dashboard_snapshot()
  list(
    ok = TRUE,
    action = "remove",
    task_id = task_id,
    message = "Task removed."
  )
}

control_clear_all_tasks <- function() {
  if (is.null(pkg_env$scheduler)) {
    return(list(
      ok = TRUE,
      action = "clear_all",
      cancelled = character(),
      removed_pending = character(),
      message = "Queue is not initialized."
    ))
  }

  slots <- as.integer(pkg_env$scheduler$capacity$slots %||% 1L)
  if (is.na(slots) || slots < 1L) {
    slots <- 1L
  }

  running_ids <- names(pkg_env$scheduler$running %||% list())
  pending_items <- pkg_env$scheduler$pending %||% list()
  pending_ids <- vapply(pending_items, function(item) as.character(item$id %||% ""), character(1))
  pending_ids <- pending_ids[nzchar(pending_ids)]

  stop_scheduler()

  failed <- character()
  killed_ids <- character()
  if (!is.null(pkg_env$active_tasks)) {
    active_ids <- ls(pkg_env$active_tasks, all.names = TRUE)
    for (task_id in active_ids) {
      task <- get(task_id, envir = pkg_env$active_tasks, inherits = FALSE)
      err <- tryCatch({
        task$kill()
        killed_ids <- c(killed_ids, task_id)
        NULL
      }, error = function(e) conditionMessage(e))
      if (!is.null(err)) {
        failed <- c(failed, paste0(task_id, ": ", err))
      }
    }
  }
  for (task_id in setdiff(running_ids, killed_ids)) {
    item <- pkg_env$scheduler$running[[task_id]]
    task <- item$task %||% NULL
    if (is.null(task)) {
      next
    }
    err <- tryCatch({
      task$kill()
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(err)) {
      failed <- c(failed, paste0(task_id, ": ", err))
    }
  }

  if (length(failed) > 0) {
    start_scheduler()
    stop("Failed to cancel all running tasks: ", paste(failed, collapse = "; "))
  }

  if (!is.null(pkg_env$active_tasks)) {
    rm(list = ls(pkg_env$active_tasks, all.names = TRUE), envir = pkg_env$active_tasks)
  }

  pkg_env$scheduler <- new_scheduler_state(max_slots = slots)
  write_dashboard_snapshot()
  list(
    ok = TRUE,
    action = "clear_all",
    cancelled = running_ids,
    removed_pending = pending_ids,
    message = "All tasks cleared."
  )
}

# Clean finished task records from the main console scheduler.
# Args:
# - None.
# Returns:
# - List payload for the dashboard control response.
# Side effects:
# - Deletes result files owned by finished records and rewrites the dashboard
#   snapshot through `clean_tasks()`.
control_clean_finished_tasks <- function() {
  if (is.null(pkg_env$scheduler)) {
    return(list(
      ok = TRUE,
      action = "clean_finished",
      removed = 0L,
      message = "Queue is not initialized."
    ))
  }

  removed <- length(pkg_env$scheduler$finished %||% list())
  clean_tasks()
  list(
    ok = TRUE,
    action = "clean_finished",
    removed = as.integer(removed),
    message = sprintf("Finished tasks cleaned (%d removed).", removed)
  )
}

handle_control_request <- function(req) {
  method <- as.character(req$REQUEST_METHOD %||% "GET")
  path <- as.character(req$PATH_INFO %||% "/")

  if (identical(method, "OPTIONS")) {
    return(control_options_response())
  }

  if (identical(method, "GET") && identical(path, "/health")) {
    return(control_json_response(payload = list(ok = TRUE, status = "running")))
  }

  if (!identical(method, "POST")) {
    return(control_json_response(status = 405L, payload = list(ok = FALSE, error = "Only POST is supported.")))
  }

  tryCatch(
    {
      payload <- parse_control_json_body(req)
      validate_control_token(payload)

      if (identical(path, "/cancel")) {
        task_id <- as.character(payload$task_id %||% "")
        if (!nzchar(task_id)) {
          stop("`task_id` is required.")
        }
        return(control_json_response(payload = control_cancel_task(task_id)))
      }

      if (identical(path, "/remove")) {
        task_id <- as.character(payload$task_id %||% "")
        if (!nzchar(task_id)) {
          stop("`task_id` is required.")
        }
        return(control_json_response(payload = control_remove_task(task_id)))
      }

      if (identical(path, "/clear_all")) {
        return(control_json_response(payload = control_clear_all_tasks()))
      }

      if (identical(path, "/clean_finished")) {
        return(control_json_response(payload = control_clean_finished_tasks()))
      }

      control_json_response(status = 404L, payload = list(ok = FALSE, error = "Unknown control endpoint."))
    },
    error = function(e) {
      control_json_response(status = 500L, payload = list(ok = FALSE, error = conditionMessage(e)))
    }
  )
}

start_control_server <- function() {
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    stop("`httpuv` is required for dashboard control. Install it with install.packages('httpuv').")
  }

  if (control_server_is_running()) {
    schedule_control_service_loop()
    return(invisible(control_server_url()))
  }

  port <- as.integer(httpuv::randomPort(host = "127.0.0.1"))
  srv <- httpuv::startServer(
    host = "127.0.0.1",
    port = port,
    app = list(call = handle_control_request),
    quiet = TRUE
  )

  pkg_env$control_server <- srv
  pkg_env$control_port <- port
  pkg_env$control_url <- sprintf("http://127.0.0.1:%d", port)
  control_server_token()
  schedule_control_service_loop(delay = 0)
  invisible(pkg_env$control_url)
}

ensure_control_server <- function() {
  if (!control_server_is_running()) {
    start_control_server()
  }
  schedule_control_service_loop()
  invisible(control_server_url())
}

stop_control_server <- function() {
  stop_control_service_loop()
  srv <- pkg_env$control_server %||% NULL
  if (!is.null(srv) && is.function(srv$stop)) {
    try(srv$stop(), silent = TRUE)
  }
  pkg_env$control_server <- NULL
  pkg_env$control_url <- NULL
  pkg_env$control_port <- NULL
  invisible(NULL)
}
