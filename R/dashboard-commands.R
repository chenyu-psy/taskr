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

  dir.create(pkg_env$dashboard_session_dir, showWarnings = FALSE, recursive = TRUE)
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
