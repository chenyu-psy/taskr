# Dashboard Runtime Paths (Internal)
#
# Purpose:
# - Keep dashboard session identifiers and snapshot paths in one place.
# - Provide a small atomic JSON writer shared by snapshot/control code.

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
  pkg_env$dashboard_cancel_dir <- file.path(pkg_env$tempdir, "cancel")

  dir.create(pkg_env$dashboard_session_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(pkg_env$dashboard_cancel_dir, showWarnings = FALSE, recursive = TRUE)
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

# Return the directory used for dashboard cancel marker files.
# Args:
# - None.
# Returns:
# - Character path. The directory is initialized when needed.
# Side effects:
# - May initialize dashboard IPC paths if the queue has not done so yet.
dashboard_cancel_dir <- function() {
  path <- pkg_env$dashboard_cancel_dir %||% NULL
  if (is.null(path) || !is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    init_dashboard_ipc(reset_session = FALSE)
    path <- pkg_env$dashboard_cancel_dir
  }
  path
}

# Build the cancel marker path for one task.
# Args:
# - task_id: Task id to cancel.
# - cancel_dir: Directory where dashboard cancel markers are stored.
# Returns:
# - Character path for the task-specific marker file.
dashboard_cancel_marker_path <- function(task_id, cancel_dir = dashboard_cancel_dir()) {
  validate_id_or_label(task_id)
  file.path(cancel_dir, paste0(task_id, ".json"))
}

# Record that the dashboard requested cancellation for one task.
# Args:
# - task_id: Task id to cancel.
# - cancel_dir: Directory where dashboard cancel markers are stored.
# Returns:
# - Invisibly returns the marker path.
# Side effects:
# - Writes one small JSON marker file atomically.
write_dashboard_cancel_marker <- function(task_id, cancel_dir = dashboard_cancel_dir()) {
  path <- dashboard_cancel_marker_path(task_id = task_id, cancel_dir = cancel_dir)
  payload <- list(
    task_id = task_id,
    requested_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  write_json_atomic(payload = payload, path = path)
  invisible(path)
}

# Check whether the dashboard requested cancellation for one task.
# Args:
# - task_id: Task id to check.
# - cancel_dir: Directory where dashboard cancel markers are stored.
# Returns:
# - TRUE when the marker file exists, otherwise FALSE.
dashboard_cancel_requested <- function(task_id, cancel_dir = dashboard_cancel_dir()) {
  file.exists(dashboard_cancel_marker_path(task_id = task_id, cancel_dir = cancel_dir))
}

# Remove the dashboard cancel marker after the scheduler has consumed it.
# Args:
# - task_id: Task id whose marker should be removed.
# - cancel_dir: Directory where dashboard cancel markers are stored.
# Returns:
# - Invisibly returns NULL.
# Side effects:
# - Deletes the marker file if it exists.
clear_dashboard_cancel_marker <- function(task_id, cancel_dir = dashboard_cancel_dir()) {
  path <- dashboard_cancel_marker_path(task_id = task_id, cancel_dir = cancel_dir)
  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }
  invisible(NULL)
}
