# Dashboard Command Inbox (Internal)
#
# Purpose:
# - Let the read-only dashboard process request queue actions without mutating
#   scheduler state directly.
# - Use one JSON file per request plus one JSON ack per result, so command
#   writes are atomic and easy to inspect.

new_dashboard_session_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%OS6"),
    "_",
    Sys.getpid(),
    "_",
    sprintf("%06d", sample.int(999999L, 1))
  )
}

init_dashboard_ipc <- function(reset_session = FALSE) {
  if (isTRUE(reset_session) || is.null(pkg_env$queue_session_id) || !nzchar(pkg_env$queue_session_id)) {
    pkg_env$queue_session_id <- new_dashboard_session_id()
  }

  pkg_env$dashboard_session_dir <- file.path(pkg_env$tempdir, "sessions", pkg_env$queue_session_id)
  pkg_env$dashboard_snapshot_path <- file.path(pkg_env$tempdir, "snapshot.json")
  pkg_env$dashboard_command_path <- file.path(pkg_env$tempdir, "commands")
  pkg_env$dashboard_ack_path <- file.path(pkg_env$tempdir, "acks")

  dir.create(pkg_env$dashboard_command_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(pkg_env$dashboard_ack_path, showWarnings = FALSE, recursive = TRUE)
  invisible(pkg_env$dashboard_session_dir)
}

dashboard_session_id <- function() {
  id <- pkg_env$queue_session_id %||% ""
  if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
    init_dashboard_ipc(reset_session = TRUE)
    id <- pkg_env$queue_session_id
  }
  id
}

dashboard_command_path <- function() {
  path <- pkg_env$dashboard_command_path %||% NULL
  if (is.null(path) || !is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    init_dashboard_ipc(reset_session = FALSE)
    path <- pkg_env$dashboard_command_path
  }
  path
}

dashboard_ack_dir <- function() {
  path <- pkg_env$dashboard_ack_path %||% NULL
  if (is.null(path) || !is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    init_dashboard_ipc(reset_session = FALSE)
    path <- pkg_env$dashboard_ack_path
  }
  path
}

ensure_dashboard_command_dir <- function(path = dashboard_command_path()) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  dir.create(dashboard_ack_dir(), showWarnings = FALSE, recursive = TRUE)
  invisible(path)
}

dashboard_request_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%OS6"),
    "_",
    Sys.getpid(),
    "_",
    sprintf("%06d", sample.int(999999L, 1))
  )
}

write_json_atomic <- function(payload, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile(pattern = paste0(basename(path), "_"), tmpdir = dirname(path), fileext = ".tmp")
  json_txt <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  writeLines(json_txt, con = tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) {
    unlink(tmp, force = TRUE)
    stop("Could not write JSON file at `", path, "`.")
  }
  invisible(path)
}

dashboard_enqueue_command <- function(
    action,
    task_id = NULL,
    path = dashboard_command_path(),
    session_id = dashboard_session_id()) {
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

  ensure_dashboard_command_dir(path)
  request_id <- dashboard_request_id()
  payload <- list(
    request_id = request_id,
    session_id = session_id,
    action = action,
    task_id = task_id,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  write_json_atomic(payload, file.path(path, paste0(request_id, ".json")))
  request_id
}

dashboard_dequeue_commands <- function(path = dashboard_command_path()) {
  ensure_dashboard_command_dir(path)
  files <- list.files(path, pattern = "[.]json$", full.names = TRUE)
  if (length(files) == 0) {
    return(list())
  }

  files <- sort(files)
  cmds <- list()
  for (file in files) {
    claim <- paste0(file, ".processing")
    if (!file.rename(file, claim)) {
      next
    }

    cmd <- tryCatch(jsonlite::fromJSON(claim, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(cmd)) {
      unlink(claim, force = TRUE)
      next
    }
    cmd$command_file <- claim
    cmds[[length(cmds) + 1L]] <- cmd
  }

  cmds
}

dashboard_write_ack <- function(cmd, status, message = "", result = NULL) {
  request_id <- as.character(cmd$request_id %||% "")
  if (!nzchar(request_id)) {
    return(invisible(NULL))
  }

  payload <- list(
    request_id = request_id,
    session_id = dashboard_session_id(),
    action = as.character(cmd$action %||% ""),
    task_id = as.character(cmd$task_id %||% ""),
    status = as.character(status),
    message = as.character(message %||% ""),
    result = result,
    processed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  write_json_atomic(payload, file.path(dashboard_ack_dir(), paste0(request_id, ".json")))
  invisible(payload)
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

process_one_dashboard_command <- function(cmd) {
  expected_session <- dashboard_session_id()
  got_session <- as.character(cmd$session_id %||% "")
  if (!identical(got_session, expected_session)) {
    dashboard_write_ack(
      cmd,
      status = "ignored",
      message = "Command belongs to a different queue session.",
      result = "session_mismatch"
    )
    return(FALSE)
  }

  action <- as.character(cmd$action %||% "")
  if (identical(action, "cancel_task")) {
    task_id <- as.character(cmd$task_id %||% "")
    if (!nzchar(task_id)) {
      dashboard_write_ack(cmd, status = "error", message = "`task_id` is required.", result = "missing_task_id")
      return(FALSE)
    }

    tryCatch(
      {
        cancel_task(task_id)
        dashboard_write_ack(cmd, status = "ok", message = "Task cancellation processed.", result = "cancelled")
        TRUE
      },
      warning = function(w) {
        dashboard_write_ack(cmd, status = "ok", message = conditionMessage(w), result = "already_terminal")
        TRUE
      },
      error = function(e) {
        dashboard_write_ack(cmd, status = "error", message = conditionMessage(e), result = "cancel_failed")
        FALSE
      }
    )
  } else if (identical(action, "clear_all")) {
    tryCatch(
      {
        dashboard_clear_all_tasks()
        dashboard_write_ack(cmd, status = "ok", message = "All tasks cleared.", result = "cleared")
        TRUE
      },
      error = function(e) {
        dashboard_write_ack(cmd, status = "error", message = conditionMessage(e), result = "clear_failed")
        FALSE
      }
    )
  } else {
    dashboard_write_ack(cmd, status = "error", message = paste("Unsupported dashboard action:", action), result = "bad_action")
    FALSE
  }
}

process_dashboard_commands <- function(path = dashboard_command_path()) {
  cmds <- dashboard_dequeue_commands(path = path)
  if (length(cmds) == 0) {
    return(invisible(FALSE))
  }

  processed_any <- FALSE
  for (cmd in cmds) {
    ok <- process_one_dashboard_command(cmd)
    processed_any <- isTRUE(processed_any) || isTRUE(ok)
    unlink(cmd$command_file %||% "", force = TRUE)
  }

  invisible(processed_any)
}
