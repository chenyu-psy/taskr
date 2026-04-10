# Queue Lifecycle Helpers (User-Facing)
#
# Purpose:
# - Initialize and reset the in-memory scheduler state.
# - Provide an emergency shutdown path that kills active tasks and clears
#   scheduler state.

new_scheduler_state <- function(max_concurrent = 1L) {
  list(
    capacity = list(slots = as.integer(max_concurrent)),
    queue = list(),
    running = list(),
    done = list(),
    next_id = 1L,
    label_index = list(),
    scheduler_handle = NULL,
    scheduler_interval = 1.0
  )
}

clear_task_tempdir <- function() {
  if (is.null(pkg_env$tempdir) || !dir.exists(pkg_env$tempdir)) {
    return(invisible(NULL))
  }

  unlink(pkg_env$tempdir, recursive = TRUE, force = TRUE)
  dir.create(pkg_env$tempdir, showWarnings = FALSE, recursive = TRUE)
  invisible(NULL)
}

default_queue_slots <- function() {
  cores <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
  if (!is.numeric(cores) || length(cores) != 1 || is.na(cores)) {
    return(4L)
  }

  slots <- as.integer(cores) - 1L
  max(1L, slots)
}

ensure_queue_initialized <- function() {
  if (!is.null(pkg_env$scheduler)) {
    return(invisible(NULL))
  }

  init_queue(max_concurrent = default_queue_slots())
  invisible(NULL)
}

#' Initialize or Reset the Task Queue
#'
#' Purpose:
#' - Create a new in-memory scheduler state for task submission.
#' - If a scheduler already exists, it is shut down first to avoid stale state.
#'
#' @param max_concurrent Integer number of task slots available at once.
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_concurrent = 2)
#' @export
init_queue <- function(max_concurrent = 1L) {
  if (!is.numeric(max_concurrent) || length(max_concurrent) != 1 || is.na(max_concurrent)) {
    stop("`max_concurrent` must be a single positive integer value.")
  }

  max_concurrent <- as.integer(max_concurrent)
  if (max_concurrent < 1L) {
    stop("`max_concurrent` must be >= 1.")
  }

  if (!is.null(pkg_env$scheduler)) {
    shutdown_queue()
  }

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = max_concurrent)
  invisible(NULL)
}

#' Shutdown the Task Queue and Cleanup Runtime State
#'
#' Purpose:
#' - Kill all active tasks, clear scheduler memory state, and remove temporary
#'   task files.
#'
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_concurrent = 1)
#' shutdown_queue()
#' @export
shutdown_queue <- function() {
  if (!is.null(pkg_env$scheduler)) {
    stop_scheduler_internal()
  }

  cleanup_active_tasks()

  if (!is.null(pkg_env$scheduler)) {
    running_items <- pkg_env$scheduler$running %||% list()
    if (length(running_items) > 0) {
      for (item in running_items) {
        if (!is.null(item$task)) {
          try(item$task$kill(), silent = TRUE)
        }
      }
    }
  }

  pkg_env$scheduler <- NULL

  if (!is.null(pkg_env$active_tasks)) {
    rm(list = ls(pkg_env$active_tasks, all.names = TRUE), envir = pkg_env$active_tasks)
  }

  clear_task_tempdir()
  invisible(NULL)
}
