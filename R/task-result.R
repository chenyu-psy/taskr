# Task Result API (User-Facing)
#
# Purpose:
# - Provide a blocking result reader for one task selected by id.

#' Read the Result of a Task by Id
#'
#' Purpose:
#' - Block until one task reaches a terminal state and return its result.
#'
#' @param id Numeric task id used to identify one task.
#' @return The stored task result object, or `NULL` when output saving is off.
#' @examples
#' init_queue(max_slots = 1)
#' # get_task_result(1)
#' @export
get_task_result <- function(id) {
  id <- normalize_task_id(id)

  if (is.null(pkg_env$scheduler)) {
    stop("Queue is not initialized. Call `init_queue()` first.")
  }

  repeat {
    pkg_env$scheduler <- update_queue(pkg_env$scheduler)
    match <- resolve_task_reference(pkg_env$scheduler, id = id)
    item <- if (is.null(match)) NULL else match$item

    if (is.null(item)) {
      stop("Task not found for `id = ", id, "`.")
    }

    status <- item$status %||% "pending"

    if (status %in% c("pending", "running")) {
      Sys.sleep(0.05)
      next
    }

    if (identical(status, "completed")) {
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
