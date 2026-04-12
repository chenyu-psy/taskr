# Dashboard Command Queue (Internal)
#
# Purpose:
# - Allow a background dashboard process to request control actions from the
#   main R session without directly mutating queue state.
# - Keep control actions simple: cancel one task or clear all tasks.

dashboard_command_path <- function() {
  path <- pkg_env$dashboard_command_path %||% NULL
  if (is.null(path) || !is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("Dashboard command path is not initialized.")
  }
  path
}

ensure_dashboard_command_file <- function(path = dashboard_command_path()) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(path)) {
    file.create(path)
  }
  invisible(path)
}

dashboard_enqueue_command <- function(action, task_id = NULL, path = dashboard_command_path()) {
  action <- as.character(action %||% "")
  if (!action %in% c("cancel_task", "clear_all")) {
    stop("Unsupported dashboard action: ", action)
  }

  if (identical(action, "cancel_task")) {
    if (is.null(task_id) || !is.character(task_id) || length(task_id) != 1 || is.na(task_id) || !nzchar(task_id)) {
      stop("`task_id` is required for `cancel_task` action.")
    }
  } else {
    task_id <- NULL
  }

  ensure_dashboard_command_file(path)
  payload <- list(
    request_id = paste0(
      format(Sys.time(), "%Y%m%d%H%M%OS6"),
      "_",
      sprintf("%06d", sample.int(999999L, 1))
    ),
    action = action,
    task_id = task_id,
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  line <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  cat(line, "\n", file = path, append = TRUE, sep = "")
  payload$request_id
}

dashboard_dequeue_commands <- function(path = dashboard_command_path()) {
  ensure_dashboard_command_file(path)
  info <- file.info(path)
  if (is.na(info$size) || info$size <= 0) {
    return(list())
  }

  consume_path <- paste0(path, ".consume")
  renamed <- file.rename(path, consume_path)
  if (!isTRUE(renamed)) {
    return(list())
  }

  file.create(path)
  lines <- tryCatch(readLines(consume_path, warn = FALSE), error = function(e) character())
  unlink(consume_path, force = TRUE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    return(list())
  }

  cmds <- vector("list", length(lines))
  n <- 0L
  for (line in lines) {
    cmd <- tryCatch(jsonlite::fromJSON(line, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(cmd)) {
      next
    }
    n <- n + 1L
    cmds[[n]] <- cmd
  }
  cmds[seq_len(n)]
}

dashboard_clear_all_tasks <- function() {
  if (is.null(pkg_env$scheduler)) {
    return(invisible(FALSE))
  }

  slots <- as.integer(pkg_env$scheduler$capacity$slots %||% 1L)
  if (is.na(slots) || slots < 1L) {
    slots <- 1L
  }

  stop_scheduler()
  cleanup_active_tasks()
  if (!is.null(pkg_env$active_tasks)) {
    rm(list = ls(pkg_env$active_tasks, all.names = TRUE), envir = pkg_env$active_tasks)
  }

  pkg_env$scheduler <- new_scheduler_state(max_slots = slots)
  write_dashboard_snapshot()
  invisible(TRUE)
}

process_dashboard_commands <- function(path = dashboard_command_path()) {
  cmds <- dashboard_dequeue_commands(path = path)
  if (length(cmds) == 0) {
    return(invisible(FALSE))
  }

  processed_any <- FALSE
  for (cmd in cmds) {
    action <- as.character(cmd$action %||% "")
    if (identical(action, "cancel_task")) {
      task_id <- as.character(cmd$task_id %||% "")
      if (nzchar(task_id)) {
        try(cancel_task(task_id), silent = TRUE)
        processed_any <- TRUE
      }
      next
    }

    if (identical(action, "clear_all")) {
      dashboard_clear_all_tasks()
      processed_any <- TRUE
      next
    }
  }

  invisible(processed_any)
}
