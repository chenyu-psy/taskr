# Task Result API (User-Facing)
#
# Purpose:
# - Provide a blocking result reader for one task selected by id or label.

validate_id_or_label <- function(id_or_label) {
  if (!is.character(id_or_label) || length(id_or_label) != 1 || is.na(id_or_label) || !nzchar(id_or_label)) {
    stop("`id_or_label` must be a single non-empty character string.")
  }

  invisible(NULL)
}

#' Read the Result of a Task by Id or Label
#'
#' Purpose:
#' - Block until one task reaches a terminal state and return its result.
#'
#' @param id_or_label Task id or label used to identify one task.
#' @return The stored task result object, or `NULL` when output saving is off.
#' @examples
#' init_queue(max_concurrent = 1)
#' # task_result("task_001")
#' @export
task_result <- function(id_or_label) {
  validate_id_or_label(id_or_label)

  if (is.null(pkg_env$scheduler)) {
    stop("Queue is not initialized. Call `init_queue()` first.")
  }

  repeat {
    pkg_env$scheduler <- tick(pkg_env$scheduler)
    match <- resolve_task_reference(pkg_env$scheduler, id_or_label = id_or_label)
    item <- if (is.null(match)) NULL else match$item

    if (is.null(item)) {
      stop("Task not found for `id_or_label = ", id_or_label, "`.")
    }

    status <- item$status %||% "queued"

    if (status %in% c("queued", "running")) {
      Sys.sleep(0.05)
      next
    }

    if (identical(status, "done")) {
      if (identical(item$output %||% "all", "none")) {
        warning("Task completed with `output = \"none\"`; no result file to read.")
        return(NULL)
      }

      path <- item$result_path %||% task_tmpfile(item$id)
      return(read_task_result_file(path))
    }

    if (identical(status, "failed")) {
      msg <- item$error %||% "Task failed."
      stop(msg)
    }

    if (identical(status, "cancelled")) {
      stop("Task was cancelled before producing a result.")
    }

    stop("Task has unsupported status: ", status)
  }
}
