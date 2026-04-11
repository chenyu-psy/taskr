# Task Control API (User-Facing)
#
# Purpose:
# - Cancel queued/running tasks by id or label.
# - Clean terminal task records and temporary result files.

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

#' Cancel One Task by Id or Label
#'
#' Purpose:
#' - Remove a queued task or kill a running task.
#' - Mark canceled tasks as `killed` in terminal records.
#'
#' @param id_or_label Task id or label used to identify one task.
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_concurrent = 1)
#' # cancel_task("task_001")
#' @export
cancel_task <- function(id_or_label) {
  validate_id_or_label(id_or_label)

  if (is.null(pkg_env$scheduler)) {
    stop("Queue is not initialized. Call `init_queue()` first.")
  }

  pkg_env$scheduler <- recycle_running_tasks(pkg_env$scheduler, now = Sys.time())
  matched <- resolve_task_reference(pkg_env$scheduler, id_or_label)
  if (is.null(matched)) {
    stop("Task not found for `id_or_label = ", id_or_label, "`.")
  }
  item <- matched$item
  now <- Sys.time()

  if (identical(matched$bucket, "done")) {
    warning("Task is already terminal; no cancellation was applied.")
    return(invisible(NULL))
  }

  if (identical(matched$bucket, "queue")) {
    pkg_env$scheduler$queue <- pkg_env$scheduler$queue[-matched$index]
  } else if (identical(matched$bucket, "running")) {
    run_id <- matched$index
    run_item <- pkg_env$scheduler$running[[run_id]]
    if (!is.null(run_item$task)) {
      try(run_item$task$kill(), silent = TRUE)
    }
    pkg_env$scheduler$running[[run_id]] <- NULL
  }

  item$status <- "killed"
  item$end_time <- now
  item$error <- item$error %||% "Task canceled by user."
  pkg_env$scheduler$done[[item$id]] <- item
  delete_task_result_file(item)
  if (!scheduler_has_work(pkg_env$scheduler)) {
    stop_scheduler_internal()
  }
  write_dashboard_snapshot()

  invisible(NULL)
}

#' Clean Terminal Task Records and Temporary Files
#'
#' Purpose:
#' - Remove done/failed/killed tasks from scheduler memory.
#' - Delete their result files when present.
#'
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_concurrent = 1)
#' clean_tasks()
#' @export
clean_tasks <- function() {
  if (is.null(pkg_env$scheduler)) {
    return(invisible(NULL))
  }

  done_items <- pkg_env$scheduler$done %||% list()
  if (length(done_items) == 0) {
    return(invisible(NULL))
  }

  for (item in done_items) {
    delete_task_result_file(item)
    pkg_env$scheduler <- remove_label_index_entry(pkg_env$scheduler, item)
  }

  pkg_env$scheduler$done <- list()
  if (!scheduler_has_work(pkg_env$scheduler)) {
    stop_scheduler_internal()
  }
  write_dashboard_snapshot()
  invisible(NULL)
}
