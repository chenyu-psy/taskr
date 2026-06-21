# Task Control API (User-Facing)
#
# Purpose:
# - Cancel pending/running tasks by id.
# - Remove one task or clean terminal task records and temporary result files.

delete_task_result_file <- function(item) {
  path <- item$result_path %||% NULL
  if (is.null(path) || !is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(invisible(NULL))
  }

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }
  invisible(NULL)
}

cancel_running_task_or_error <- function(task_obj, id) {
  if (is.null(task_obj)) {
    stop("Running task `", id, "` has no task object; cannot cancel safely.")
  }

  kill_err <- NULL
  tryCatch(
    task_obj$kill(),
    error = function(e) {
      kill_err <<- conditionMessage(e)
    }
  )

  if (!is.null(kill_err)) {
    stop("Failed to cancel running task `", id, "`: ", kill_err)
  }

  invisible(NULL)
}

get_active_task_by_id <- function(id) {
# Return a still-registered task object by id.
#
# Purpose:
# - Provide a last-resort lookup when scheduler state is stale or missing.
#
# Parameters:
# - `id`: Internal task id, not a label.
#
# Returns:
# - Task object when it is still registered; otherwise `NULL`.
  id <- task_id_key(id)
  if (is.null(pkg_env$active_tasks)) {
    return(NULL)
  }
  if (!exists(id, envir = pkg_env$active_tasks, inherits = FALSE)) {
    return(NULL)
  }

  get(id, envir = pkg_env$active_tasks, inherits = FALSE)
}

active_task_is_alive <- function(task_obj) {
# Report whether a registry task still has a live child process.
#
# Purpose:
# - Distinguish a harmless stale terminal record from a real leaked process.
#
# Parameters:
# - `task_obj`: Internal task object or process-like test double.
#
# Returns:
# - `logical(1)`: `TRUE` only when the task reports it is alive.
  if (is.null(task_obj) || is.null(task_obj$is_alive) || !is.function(task_obj$is_alive)) {
    return(FALSE)
  }

  isTRUE(tryCatch(task_obj$is_alive(), error = function(e) FALSE))
}

cancel_active_task_by_id <- function(id) {
# Kill a registered task when scheduler lookup cannot find it.
#
# Purpose:
# - Release compute resources even if queue/running/finished state became
#   stale before the user requested cancellation.
#
# Parameters:
# - `id`: Internal task id.
#
# Returns:
# - `TRUE` when a registered task was killed, otherwise `FALSE`.
  id <- normalize_task_id(id)
  task_obj <- get_active_task_by_id(id)
  if (is.null(task_obj)) {
    return(FALSE)
  }

  cancel_running_task_or_error(task_obj = task_obj, id = id)
  unregister_active_task(id)
  TRUE
}

#' Cancel One or More Tasks by Id
#'
#' Purpose:
#' - Remove pending tasks or kill running tasks.
#' - Mark canceled tasks as `cancelled` in terminal records.
#'
#' @param id Numeric task id vector.
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_slots = 1)
#' # cancel_task(1)
#' @export
cancel_task <- function(id) {
  ids <- normalize_task_ids(id, allow_multiple = TRUE)
  for (task_id in ids) {
    cancel_one_task(task_id)
  }

  invisible(NULL)
}

cancel_one_task <- function(id) {
  if (is.null(pkg_env$scheduler)) {
    if (cancel_active_task_by_id(id)) {
      write_dashboard_snapshot()
      return(invisible(NULL))
    }
    stop("Queue is not initialized. Call `init_queue()` first.")
  }

  pkg_env$scheduler <- recycle_running_tasks(pkg_env$scheduler, now = Sys.time())
  matched <- resolve_task_reference(pkg_env$scheduler, id)
  if (is.null(matched)) {
    if (cancel_active_task_by_id(id)) {
      write_dashboard_snapshot()
      return(invisible(NULL))
    }
    stop("Task not found for `id = ", id, "`.")
  }
  item <- matched$item
  now <- Sys.time()

  if (identical(matched$bucket, "finished")) {
    active_task <- get_active_task_by_id(item$id)
    if (active_task_is_alive(active_task)) {
      cancel_running_task_or_error(task_obj = active_task, id = item$id)
      unregister_active_task(item$id)
      item$status <- "cancelled"
      item$end_time <- now
      item$error <- item$error %||% "Task canceled by user."
      pkg_env$scheduler$finished[[task_id_key(item$id)]] <- item
      delete_task_result_file(item)
      write_dashboard_snapshot()
      return(invisible(NULL))
    }

    warning("Task is already terminal; no cancellation was applied.")
    return(invisible(NULL))
  }

  if (identical(matched$bucket, "pending")) {
    pkg_env$scheduler$pending <- pkg_env$scheduler$pending[-matched$index]
  } else if (identical(matched$bucket, "running")) {
    run_id <- matched$index
    run_item <- pkg_env$scheduler$running[[run_id]]
    cancel_running_task_or_error(task_obj = run_item$task, id = run_item$id %||% run_id)
    pkg_env$scheduler$running[[run_id]] <- NULL
  }

  item$status <- "cancelled"
  item$end_time <- now
  item$error <- item$error %||% "Task canceled by user."
  pkg_env$scheduler$finished[[task_id_key(item$id)]] <- item
  delete_task_result_file(item)
  pkg_env$scheduler <- update_queue(pkg_env$scheduler, now = now)
  if (!scheduler_has_work(pkg_env$scheduler)) {
    stop_scheduler()
  } else {
    start_scheduler()
  }
  write_dashboard_snapshot()

  invisible(NULL)
}

remove_finished_task_record <- function(id) {
# Remove one terminal task record from scheduler memory.
#
# Purpose:
# - Keep single-task removal separate from bulk cleanup.
#
# Parameters:
# - `id`: Internal task id for an item in `scheduler$finished`.
#
# Returns:
# - The removed task item.
#
# Assumptions and side effects:
# - Deletes the task result file when present.
  id <- task_id_key(id)
  item <- pkg_env$scheduler$finished[[id]] %||% NULL
  if (is.null(item)) {
    stop("Finished task `", id, "` was not found.")
  }

  delete_task_result_file(item)
  pkg_env$scheduler$finished[[id]] <- NULL
  item
}

#' Remove One or More Tasks by Id
#'
#' Purpose:
#' - Remove one task record from the queue monitor.
#' - Cancel pending or running work before removing its terminal record.
#'
#' @param id Numeric task id vector.
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_slots = 1)
#' # remove_task(1)
#' @export
remove_task <- function(id) {
  ids <- normalize_task_ids(id, allow_multiple = TRUE)
  for (task_id in ids) {
    remove_one_task(task_id)
  }

  invisible(NULL)
}

remove_one_task <- function(id) {
  if (is.null(pkg_env$scheduler)) {
    if (cancel_active_task_by_id(id)) {
      write_dashboard_snapshot()
      return(invisible(NULL))
    }
    stop("Queue is not initialized. Call `init_queue()` first.")
  }

  pkg_env$scheduler <- recycle_running_tasks(pkg_env$scheduler, now = Sys.time())
  matched <- resolve_task_reference(pkg_env$scheduler, id)
  if (is.null(matched)) {
    if (cancel_active_task_by_id(id)) {
      write_dashboard_snapshot()
      return(invisible(NULL))
    }
    stop("Task not found for `id = ", id, "`.")
  }

  task_id <- matched$item$id
  if (!identical(matched$bucket, "finished")) {
    cancel_one_task(task_id)
  }

  pkg_env$scheduler <- recycle_running_tasks(pkg_env$scheduler, now = Sys.time())
  matched <- resolve_task_reference(pkg_env$scheduler, task_id)
  if (is.null(matched) || !identical(matched$bucket, "finished")) {
    stop("Task `", task_id, "` could not be removed after cancellation.")
  }

  remove_finished_task_record(task_id)
  if (!scheduler_has_work(pkg_env$scheduler)) {
    stop_scheduler()
  }
  write_dashboard_snapshot()

  invisible(NULL)
}

#' Clean Terminal Task Records and Temporary Files
#'
#' Purpose:
#' - Remove completed/failed/cancelled tasks from scheduler memory.
#' - Delete their result files when present.
#'
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_slots = 1)
#' clean_tasks()
#' @export
clean_tasks <- function() {
  if (is.null(pkg_env$scheduler)) {
    return(invisible(NULL))
  }

  finished_items <- pkg_env$scheduler$finished %||% list()
  if (length(finished_items) == 0) {
    return(invisible(NULL))
  }

  for (item in finished_items) {
    delete_task_result_file(item)
  }

  pkg_env$scheduler$finished <- list()
  if (!scheduler_has_work(pkg_env$scheduler)) {
    stop_scheduler()
  }
  write_dashboard_snapshot()
  invisible(NULL)
}
