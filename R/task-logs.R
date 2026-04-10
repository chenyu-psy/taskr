# Task Logs API (User-Facing)
#
# Purpose:
# - Return captured stdout/stderr text for one task.

#' Read Captured Logs for One Task
#'
#' Purpose:
#' - Return buffered `stdout` and `stderr` for a task identified by id or label.
#'
#' @param id_or_label Task id or label used to identify one task.
#' @return A named list with `id`, `label`, `status`, `stdout`, and `stderr`.
#' @examples
#' init_queue(max_concurrent = 1)
#' # task_logs("task_001")
#' @export
task_logs <- function(id_or_label) {
  validate_id_or_label(id_or_label)

  if (is.null(pkg_env$scheduler)) {
    stop("Queue is not initialized. Call `init_queue()` first.")
  }

  pkg_env$scheduler <- recycle_running_tasks(pkg_env$scheduler, now = Sys.time())
  match <- resolve_task_reference(pkg_env$scheduler, id_or_label = id_or_label)
  if (is.null(match)) {
    stop("Task not found for `id_or_label = ", id_or_label, "`.")
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
