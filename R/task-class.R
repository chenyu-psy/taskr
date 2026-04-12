# Internal Task Class for Background Process State
#
# This file defines the internal `Task` R6 class used by `taskr` to track one
# background task. At this stage the class only stores task metadata and a
# process-like object. Later steps will connect it to real `callr` sessions.
#
# The class is intentionally not exported. Users should interact with tasks
# through package functions such as `submit_code()` and `get_task_overview()`.
Task <- R6::R6Class(
  classname = "Task",
  public = list(
    id = NULL,
    created_at = NULL,
    process = NULL,
    status_value = NULL,
    finished_at = NULL,
    started_at = NULL,
    stdout_buffer = NULL,
    stderr_buffer = NULL,
    progress_state = NULL,
    result_path = NULL,
    error = NULL,
    call_result = NULL,

    initialize = function(id, process = NULL, status = "queued", result_path = NULL) {
# Create a new internal task object.
#
# Purpose:
# - Store basic task metadata in one mutable object.
#
# Parameters:
# - `id`: Task identifier used by the package registry.
# - `process`: Optional process-like object with `is_alive()` and
#   `kill()` methods.
# - `status`: Initial task status string.
#
# Returns:
# - The modified task object.
#
# Assumptions and side effects:
# - Assumes `id` is a single non-missing character string.
# - Records the creation time immediately.
      if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
        stop("`id` must be a single non-empty character string.")
      }

      allowed_status <- c("queued", "running", "completed", "failed", "cancelled")
      if (!is.character(status) || length(status) != 1 || is.na(status)) {
        stop("`status` must be a single non-missing character string.")
      }
      if (!status %in% allowed_status) {
        stop("`status` must be one of: ", paste(allowed_status, collapse = ", "))
      }

      self$id <- id
      self$process <- process
      self$status_value <- status
      self$created_at <- Sys.time()
      self$stdout_buffer <- ""
      self$stderr_buffer <- ""
      self$progress_state <- NULL
      self$result_path <- result_path
      self$error <- NULL
      self$call_result <- NULL

      if (identical(status, "running")) {
        self$started_at <- self$created_at
      }

      if (status %in% c("completed", "failed", "cancelled")) {
        self$finished_at <- self$created_at
      }

      invisible(self)
    },

    set_status = function(status) {
# Update the stored task status.
#
# Purpose:
# - Keep status bookkeeping in one place for internal code.
#
# Parameters:
# - `status`: New task status string.
#
# Returns:
# - Invisibly returns the task object.
#
# Assumptions and side effects:
# - Updates `started_at` when a task first enters `running`.
# - Updates `finished_at` when a task enters a terminal state.
      allowed_status <- c("queued", "running", "completed", "failed", "cancelled")
      if (!is.character(status) || length(status) != 1 || is.na(status)) {
        stop("`status` must be a single non-missing character string.")
      }
      if (!status %in% allowed_status) {
        stop("`status` must be one of: ", paste(allowed_status, collapse = ", "))
      }

      self$status_value <- status

      if (identical(status, "running") && is.null(self$started_at)) {
        self$started_at <- Sys.time()
      }

      if (status %in% c("completed", "failed", "cancelled")) {
        self$finished_at <- Sys.time()
      }

      invisible(self)
    },

    status = function() {
# Return the current task status.
#
# Purpose:
# - Provide a simple accessor so upper layers do not read fields
#   directly.
#
# Returns:
# - `character(1)`: Current task status.
      if (identical(self$status_value, "cancelled")) {
        return(self$status_value)
      }

      if (is.null(self$process)) {
        return(self$status_value)
      }

      task_update_call_result(self)

      session_state <- NULL
      if (!is.null(self$process$get_state) && is.function(self$process$get_state)) {
        session_state <- self$process$get_state()
      }

      if (identical(self$status_value, "running") && !is.null(self$call_result)) {
        if (!is.null(self$call_result$error)) {
          self$error <- task_result_error_message(
            error = self$call_result$error,
            stderr_text = self$stderr_buffer
          )
          self$set_status("failed")
          unregister_active_task(self$id)
        } else if (!is.null(self$result_path) && file.exists(self$result_path)) {
          self$set_status("completed")
          unregister_active_task(self$id)
        } else {
          self$error <- task_process_failure_message(self)
          self$set_status("failed")
          unregister_active_task(self$id)
        }

        return(self$status_value)
      }

      if (identical(self$status_value, "running") && !is.null(session_state) &&
          session_state %in% c("idle", "finished")) {
        if (!is.null(self$result_path) && file.exists(self$result_path)) {
          self$set_status("completed")
          unregister_active_task(self$id)
        } else {
          if (is.null(self$error)) {
            self$error <- task_process_failure_message(self)
          }
          self$set_status("failed")
          unregister_active_task(self$id)
        }

        return(self$status_value)
      }

      if (self$is_alive()) {
        return("running")
      }

      if (identical(self$status_value, "running")) {
        if (!is.null(self$result_path) && file.exists(self$result_path)) {
          self$set_status("completed")
          unregister_active_task(self$id)
        } else {
          self$error <- task_process_failure_message(self)
          self$set_status("failed")
          unregister_active_task(self$id)
        }
      }

      self$status_value
    },

    progress = function() {
# Return the latest parsed progress update.
#
# Purpose:
# - Give upper layers access to the most recent progress event emitted by
#   the child process.
#
# Returns:
# - `NULL` when no progress has been reported yet, otherwise a list with
#   `fraction`, `message`, and `updated_at`.
      self$progress_state
    },

    is_alive = function() {
# Report whether the underlying process is still alive.
#
# Purpose:
# - Ask the process handle for liveness when available.
#
# Returns:
# - `logical(1)`: `TRUE` when the process reports it is alive.
#
# Assumptions and side effects:
# - If no process handle exists yet, returns `FALSE`.
      if (is.null(self$process)) {
        return(FALSE)
      }

      isTRUE(self$process$is_alive())
    },

    read_output = function() {
# Read incremental standard output and parse progress events.
#
# Purpose:
# - Pull newly available stdout from the child process, remove taskr
#   progress protocol messages, and keep ordinary output for later
#   display.
#
# Returns:
# - `character(1)`: Newly read non-protocol stdout text.
#
# Assumptions and side effects:
# - Updates `stdout_buffer` with ordinary stdout text.
# - Updates `progress_state` when progress protocol messages are found.
      chunk <- task_read_stream(self$process, stream = "output")
      parsed <- parse_task_output(chunk)

      if (length(parsed$events) > 0) {
        last_event <- parsed$events[[length(parsed$events)]]
        self$progress_state <- list(
          fraction = last_event$fraction,
          message = last_event$message,
          updated_at = last_event$ts
        )
      }

      self$stdout_buffer <- paste0(self$stdout_buffer, parsed$text)
      parsed$text
    },

    read_error = function() {
# Read incremental standard error output.
#
# Purpose:
# - Pull newly available stderr from the child process and append it to
#   the internal stderr buffer.
#
# Returns:
# - `character(1)`: Newly read stderr text.
      chunk <- task_read_stream(self$process, stream = "error")
      self$stderr_buffer <- paste0(self$stderr_buffer, chunk)
      chunk
    },

    kill = function() {
# Stop the underlying process and mark the task as cancelled.
#
# Purpose:
# - Provide one place where queue code can terminate a running task.
#
# Returns:
# - Invisibly returns the task object.
#
# Assumptions and side effects:
# - Calls `$kill()` on the process handle when present.
# - Updates status bookkeeping on the task object.
      if (!is.null(self$process) && self$is_alive()) {
        self$process$kill()
      }
      if (!is.null(self$process) && is.function(self$process$close)) {
        try(self$process$close(), silent = TRUE)
      }

      self$set_status("cancelled")
      unregister_active_task(self$id)
    },

    elapsed = function() {
# Return elapsed task time in seconds.
#
# Purpose:
# - Give upper layers a simple elapsed time measure for display.
#
# Returns:
# - `numeric(1)`: Seconds from creation until now or finish time.
      end_time <- self$finished_at
      if (is.null(end_time)) {
        end_time <- Sys.time()
      }

      as.numeric(difftime(end_time, self$created_at, units = "secs"))
    }
  ),
  private = list(
    finalize = function() {
# Finalize a task object during garbage collection.
#
# Purpose:
# - Kill any still-running child process if the task object is collected
#   before explicit cleanup happens.
#
# Returns:
# - No return value.
      try(self$kill(), silent = TRUE)
    }
  )
)

#' Parse stdout text and extract taskr progress events.
#'
#' Purpose:
#' - Separate ordinary stdout from embedded task progress protocol messages.
#'
#' Parameters:
#' - `text`: Newly read stdout text.
#'
#' Returns:
#' - A list with `text` and `events`.
#'
#' Assumptions and side effects:
#' - Leaves non-protocol stdout untouched.
#' - Parses only complete tagged protocol messages.
#'
#' @keywords internal
parse_task_output <- function(text) {
  start_tag <- "##TASKR_PROGRESS##"
  end_tag <- "##/TASKR_PROGRESS##"
  events <- list()
  clean_text <- ""
  remaining <- text

  while (nzchar(remaining)) {
    start_pos <- regexpr(start_tag, remaining, fixed = TRUE)[1]
    if (start_pos < 0) {
      clean_text <- paste0(clean_text, remaining)
      break
    }

    if (start_pos > 1) {
      clean_text <- paste0(clean_text, substr(remaining, 1, start_pos - 1))
    }

    tagged_text <- substr(remaining, start_pos, nchar(remaining))
    end_pos <- regexpr(end_tag, tagged_text, fixed = TRUE)[1]
    if (end_pos < 0) {
      clean_text <- paste0(clean_text, tagged_text)
      break
    }

    json_start <- nchar(start_tag) + 1
    json_end <- end_pos - 1
    json_text <- substr(tagged_text, json_start, json_end)
    event <- jsonlite::fromJSON(json_text, simplifyVector = TRUE)
    event$ts <- as.POSIXct(event$ts, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    events[[length(events) + 1]] <- event

    after_end <- end_pos + nchar(end_tag)
    remaining <- substr(tagged_text, after_end, nchar(tagged_text))
  }

  list(text = clean_text, events = events)
}

#' Read one output stream from a process-like object.
#'
#' Purpose:
#' - Hide process-specific output methods behind one small internal helper.
#'
#' Parameters:
#' - `process`: Optional process-like object.
#' - `stream`: One of `"output"` or `"error"`.
#'
#' Returns:
#' - `character(1)`: Newly read text, or `""` when no output is available.
#'
#' Assumptions and side effects:
#' - Supports simple fake processes used in tests.
#' - Supports either `$read_output()` / `$read_error()` or
#'   `$read_output_lines()` / `$read_error_lines()` if provided by the backend.
#'
#' @keywords internal
task_read_stream <- function(process, stream = c("output", "error")) {
  stream <- match.arg(stream)
  if (is.null(process)) {
    return("")
  }

  read_method <- paste0("read_", stream)
  read_lines_method <- paste0("read_", stream, "_lines")

  if (!is.null(process[[read_method]]) && is.function(process[[read_method]])) {
    value <- process[[read_method]]()
    if (length(value) == 0 || is.null(value)) {
      return("")
    }
    return(paste(value, collapse = ""))
  }

  if (!is.null(process[[read_lines_method]]) && is.function(process[[read_lines_method]])) {
    value <- process[[read_lines_method]]()
    if (length(value) == 0 || is.null(value)) {
      return("")
    }
    return(paste0(paste(value, collapse = "\n"), "\n"))
  }

  ""
}

#' Build a readable task failure message.
#'
#' Purpose:
#' - Convert the available process output into a simple failure message when a
#'   child process exits without producing a result file.
#'
#' Parameters:
#' - `task`: Internal `Task` object.
#'
#' Returns:
#' - `character(1)`: Failure message.
#'
#' @keywords internal
task_process_failure_message <- function(task) {
  stderr_text <- task$read_error()
  if (nzchar(stderr_text)) {
    return(trimws(stderr_text))
  }

  "subprocess died unexpectedly"
}

#' Update a task with any finished call result from a persistent session.
#'
#' Purpose:
#' - Detect when a `callr::r_session` call has completed even though the
#'   underlying R process remains alive and reusable.
#'
#' Parameters:
#' - `task`: Internal `Task` object.
#'
#' Returns:
#' - Invisibly returns the task object.
#'
#' @keywords internal
task_update_call_result <- function(task) {
  if (!identical(task$status_value, "running")) {
    return(invisible(task))
  }

  process <- task$process
  if (is.null(process)) {
    return(invisible(task))
  }

  if (!is.null(task$call_result)) {
    return(invisible(task))
  }

  if (!is.null(process$read) && is.function(process$read)) {
    repeat {
      event <- process$read()
      if (is.null(event)) {
        break
      }

      if (!is.null(event$stdout) && nzchar(event$stdout)) {
        parsed <- parse_task_output(event$stdout)
        if (length(parsed$events) > 0) {
          last_event <- parsed$events[[length(parsed$events)]]
          task$progress_state <- list(
            fraction = last_event$fraction,
            message = last_event$message,
            updated_at = last_event$ts
          )
        }
        task$stdout_buffer <- paste0(task$stdout_buffer, parsed$text)
      }

      if (!is.null(event$stderr) && nzchar(event$stderr)) {
        task$stderr_buffer <- paste0(task$stderr_buffer, event$stderr)
      }

      if (identical(event$code, 200L)) {
        task$call_result <- event
        break
      }

      if (identical(event$code, 500L) || identical(event$code, 501L) || identical(event$code, 502L)) {
        task$call_result <- list(
          code = event$code,
          result = NULL,
          stdout = "",
          stderr = task$stderr_buffer,
          error = structure(
            list(message = "subprocess died unexpectedly"),
            class = "error"
          )
        )
        break
      }
    }
  }

  invisible(task)
}

#' Convert a callr error object into a readable message.
#'
#' Purpose:
#' - Extract a concise failure message from the call result object returned by
#'   `callr::r_session$read()`.
#'
#' Parameters:
#' - `error`: Error object returned by `callr`.
#'
#' Returns:
#' - `character(1)`: Readable error message.
#'
#' @keywords internal
task_result_error_message <- function(error, stderr_text = "") {
  if (is.null(error)) {
    return(NULL)
  }

  if (nzchar(stderr_text)) {
    return(trimws(stderr_text))
  }

  if (!is.null(error$parent$message) && nzchar(error$parent$message)) {
    return(error$parent$message)
  }

  if (!is.null(error$stderr) && nzchar(error$stderr)) {
    return(trimws(error$stderr))
  }

  if (!is.null(error$message) && nzchar(error$message)) {
    return(error$message)
  }

  as.character(error)[1]
}
