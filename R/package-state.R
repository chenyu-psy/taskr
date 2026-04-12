#' Internal Package State for Temporary Task Storage
#'
#' This file defines package-level state used by the task engine. The state is
#' intentionally small and explicit: it only stores the package temp directory
#' path used for task result files.

pkg_env <- new.env(parent = emptyenv())

#' Initialize package state when `taskr` is loaded.
#'
#' Purpose:
#' - Create the package-specific temp directory used for task result files.
#'
#' Parameters:
#' - `libname`: Library path supplied by R during package loading.
#' - `pkgname`: Package name supplied by R during package loading.
#'
#' Returns:
#' - No return value. Called for its side effect during package load.
#'
#' Assumptions and side effects:
#' - Creates `tempdir()/taskr` if it does not already exist.
#' - Stores that directory path in `pkg_env$tempdir`.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  pkg_env$tempdir <- file.path(tempdir(), "taskr")
  pkg_env$active_tasks <- new.env(parent = emptyenv())
  pkg_env$scheduler <- NULL
  pkg_env$dashboard_process <- NULL
  pkg_env$dashboard_url <- NULL
  pkg_env$dashboard_port <- NULL
  pkg_env$dashboard_snapshot_path <- file.path(pkg_env$tempdir, "dashboard-snapshot.json")
  pkg_env$dashboard_command_path <- file.path(pkg_env$tempdir, "dashboard-commands.jsonl")
  dir.create(pkg_env$tempdir, showWarnings = FALSE, recursive = TRUE)
  try(write_dashboard_snapshot(), silent = TRUE)
  try(ensure_dashboard_command_file(), silent = TRUE)
}

#' Clean package state when `taskr` is unloaded.
#'
#' Purpose:
#' - Stop any still-running child processes and remove the package temp
#'   directory as a last-resort cleanup step.
#'
#' Parameters:
#' - `libpath`: Library path supplied by R during package unloading.
#'
#' Returns:
#' - No return value. Called for its side effect during package unload.
#'
#' Assumptions and side effects:
#' - Tries to kill any active tasks still tracked in `pkg_env$active_tasks`.
#' - Deletes the package temp directory recursively when it exists.
#'
#' @keywords internal
.onUnload <- function(libpath) {
  if (!is.null(pkg_env$scheduler)) {
    stop_scheduler()
    pkg_env$scheduler <- NULL
  }

  cleanup_active_tasks()
  try(stop_dashboard_background(), silent = TRUE)

  if (!is.null(pkg_env$tempdir) && dir.exists(pkg_env$tempdir)) {
    unlink(pkg_env$tempdir, recursive = TRUE, force = TRUE)
  }
}

#' Build the result file path for a task id.
#'
#' Purpose:
#' - Derive the `.rds` path for one task without storing file paths in a
#'   registry.
#'
#' Parameters:
#' - `id`: Task identifier created by `taskr`.
#'
#' Returns:
#' - `character(1)`: Full file path where the task result should be stored.
#'
#' Assumptions and side effects:
#' - Assumes `.onLoad()` has already initialized `pkg_env$tempdir`.
#' - Does not create the file; it only returns the expected path.
#'
#' @keywords internal
task_tmpfile <- function(id) {
  if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
    stop("`id` must be a single non-empty character string.")
  }

  if (is.null(pkg_env$tempdir) || !nzchar(pkg_env$tempdir)) {
    stop("Package temp directory has not been initialized.")
  }

  file.path(pkg_env$tempdir, paste0(id, ".rds"))
}

#' Register a task for package-level cleanup.
#'
#' Purpose:
#' - Keep a small list of active task objects that should be cleaned up on
#'   package unload.
#'
#' Parameters:
#' - `task`: Internal `Task` object.
#'
#' Returns:
#' - Invisibly returns the task object.
#'
#' @keywords internal
register_active_task <- function(task) {
  if (!is.null(pkg_env$active_tasks)) {
    assign(task$id, task, envir = pkg_env$active_tasks)
  }

  invisible(task)
}

#' Remove a task from the package-level cleanup registry.
#'
#' Purpose:
#' - Stop tracking a task once it reaches a terminal state.
#'
#' Parameters:
#' - `id`: Task identifier.
#'
#' Returns:
#' - Invisibly returns `NULL`.
#'
#' @keywords internal
unregister_active_task <- function(id) {
  if (!is.null(pkg_env$active_tasks) && exists(id, envir = pkg_env$active_tasks, inherits = FALSE)) {
    rm(list = id, envir = pkg_env$active_tasks)
  }

  invisible(NULL)
}

#' Kill all tracked tasks during package cleanup.
#'
#' Purpose:
#' - Provide one cleanup path for `.onUnload()` and tests.
#'
#' Returns:
#' - Invisibly returns `NULL`.
#'
#' @keywords internal
cleanup_active_tasks <- function() {
  if (is.null(pkg_env$active_tasks)) {
    return(invisible(NULL))
  }

  task_ids <- ls(pkg_env$active_tasks, all.names = TRUE)
  for (task_id in task_ids) {
    task <- get(task_id, envir = pkg_env$active_tasks, inherits = FALSE)
    try(task$kill(), silent = TRUE)
  }

  invisible(NULL)
}

#' Read a task result file with a clear missing-file error.
#'
#' Purpose:
#' - Centralize result-file reading so later user-facing APIs can reuse one
#'   readable error path when the expected temp file is missing.
#'
#' Parameters:
#' - `path`: Full path to the expected `.rds` result file.
#'
#' Returns:
#' - The object stored in the `.rds` file.
#'
#' Assumptions and side effects:
#' - Errors clearly if the file does not exist.
#'
#' @keywords internal
read_task_result_file <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single non-empty character string.")
  }

  if (!file.exists(path)) {
    stop(
      "Task result file is missing at `", path, "`. ",
      "It may have been removed manually or the task temp directory was cleaned."
    )
  }

  readRDS(path)
}
