# Task Logs API (User-Facing)
#
# Purpose:
# - Return captured stdout/stderr text for one task.

#' Read Captured Logs for One Task
#'
#' Purpose:
#' - Return buffered `stdout` and `stderr` for a task identified by id.
#'
#' @param id Numeric task id used to identify one task.
#' @return A named list with `id`, `label`, `status`, `stdout`, and `stderr`.
#' @examples
#' init_queue(max_slots = 1)
#' # get_task_log(1)
#' @export
get_task_log <- function(id) {
  id <- normalize_task_id(id)

  if (is.null(pkg_env$scheduler)) {
    stop("Queue is not initialized. Call `init_queue()` first.")
  }

  pkg_env$scheduler <- recycle_running_tasks(pkg_env$scheduler, now = Sys.time())
  match <- resolve_task_reference(pkg_env$scheduler, id = id)
  if (is.null(match)) {
    stop("Task not found for `id = ", id, "`.")
  }

  item <- match$item
  list(
    id = item$id %||% NA_character_,
    label = item$label %||% NA_character_,
    status = item$status %||% NA_character_,
    stdout = item$stdout_buffer %||% "",
    stderr = item$stderr_buffer %||% ""
  )
}
