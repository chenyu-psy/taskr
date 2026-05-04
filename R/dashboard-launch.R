# Dashboard Background Launcher (Internal)
#
# Purpose:
# - Run the task monitor in a separate R process so main console stays usable.
# - The background monitor reads session-local JSON snapshots written by the
#   main queue process.

dashboard_process_is_alive <- function(proc) {
  !is.null(proc) && is.function(proc$is_alive) && isTRUE(proc$is_alive())
}

dashboard_pick_port <- function() {
  if (requireNamespace("httpuv", quietly = TRUE)) {
    return(as.integer(httpuv::randomPort(host = "127.0.0.1")))
  }
  as.integer(sample(3000:9000, size = 1))
}

dashboard_open_viewer <- function(url, open_viewer = TRUE) {
  if (!isTRUE(open_viewer)) {
    return(invisible(FALSE))
  }

  viewer <- getOption("viewer")
  if (is.function(viewer)) {
    try(viewer(url), silent = TRUE)
    return(invisible(TRUE))
  }

  invisible(FALSE)
}

dashboard_port_ready <- function(port, timeout_sec = 5) {
  deadline <- Sys.time() + as.numeric(timeout_sec)

  repeat {
    con <- tryCatch(
      suppressWarnings(
        socketConnection(
          host = "127.0.0.1",
          port = as.integer(port),
          open = "r+",
          blocking = TRUE,
          timeout = 0.2
        )
      ),
      error = function(e) NULL
    )
    if (!is.null(con)) {
      try(close(con), silent = TRUE)
      return(TRUE)
    }
    if (Sys.time() >= deadline) {
      return(FALSE)
    }
    Sys.sleep(0.1)
  }
}

dashboard_collect_process_logs <- function(proc) {
  out <- tryCatch(proc$read_output_lines(), error = function(e) character())
  err <- tryCatch(proc$read_error_lines(), error = function(e) character())
  list(stdout = out %||% character(), stderr = err %||% character())
}

#' Check whether a directory is the taskr source package.
#'
#' Purpose:
#' - Keep dashboard development launches from accidentally loading a different
#'   R project just because the working directory has a `DESCRIPTION` file.
#'
#' Parameters:
#' - `pkg_path`: Candidate package source directory.
#'
#' Returns:
#' - `TRUE` when `pkg_path/DESCRIPTION` declares `Package: taskr` and the
#'   directory contains plain `R/*.R` source files; otherwise `FALSE`.
#'
#' Assumptions and side effects:
#' - Reads only the candidate `DESCRIPTION` file.
#' - Does not load the package.
#'
#' @keywords internal
dashboard_is_taskr_source_path <- function(pkg_path) {
  if (!is.character(pkg_path) || length(pkg_path) != 1 || is.na(pkg_path) || !nzchar(pkg_path)) {
    return(FALSE)
  }

  desc_path <- file.path(pkg_path, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    return(FALSE)
  }

  desc <- tryCatch(
    read.dcf(desc_path, fields = "Package"),
    error = function(e) matrix(character(), nrow = 0, ncol = 1)
  )
  if (nrow(desc) < 1 || ncol(desc) < 1) {
    return(FALSE)
  }

  if (!identical(as.character(desc[1, 1]), "taskr")) {
    return(FALSE)
  }

  r_files <- list.files(file.path(pkg_path, "R"), pattern = "\\.R$", full.names = TRUE)
  length(r_files) > 0
}

#' Find the local taskr source path for a dashboard child process.
#'
#' Purpose:
#' - Prefer the current source checkout during development so dashboard changes
#'   are available without reinstalling.
#' - Return `""` when only an installed package is available, so the child
#'   process uses `library(taskr)` instead of `pkgload::load_all()`.
#'
#' Returns:
#' - `character(1)`: Valid taskr source path, or `""` when none is detected.
#'
#' Assumptions and side effects:
#' - Checks `getwd()`, the loaded namespace path, and that path's parent.
#' - Does not load or modify the package.
#'
#' @keywords internal
dashboard_detect_pkg_path <- function() {
  wd_path <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (dashboard_is_taskr_source_path(wd_path)) {
    return(wd_path)
  }

  ns_path <- tryCatch(getNamespaceInfo("taskr", "path"), error = function(e) "")
  if (is.character(ns_path) && nzchar(ns_path)) {
    ns_path <- normalizePath(ns_path, winslash = "/", mustWork = FALSE)
    if (dashboard_is_taskr_source_path(ns_path)) {
      return(ns_path)
    }
    parent_path <- dirname(ns_path)
    if (dashboard_is_taskr_source_path(parent_path)) {
      return(parent_path)
    }
  }

  ""
}

#' Stop the dashboard background process tracked by taskr.
#'
#' Purpose:
#' - Clean up the Shiny dashboard process when the queue is reset, the package is
#'   unloaded, or a dashboard must be relaunched with new control settings.
#'
#' Returns:
#' - Invisibly returns `NULL`.
#'
#' Assumptions and side effects:
#' - Kills the tracked dashboard process when it is still alive.
#' - Clears dashboard process, URL, and port fields in `pkg_env`.
#'
#' @keywords internal
stop_dashboard_background <- function() {
  proc <- pkg_env$dashboard_process %||% NULL
  if (dashboard_process_is_alive(proc)) {
    try(proc$kill(), silent = TRUE)
  }
  pkg_env$dashboard_process <- NULL
  pkg_env$dashboard_url <- NULL
  pkg_env$dashboard_port <- NULL
  invisible(NULL)
}

#' Start the dashboard in a background R process.
#'
#' Purpose:
#' - Keep the user's main R session responsive while the Shiny dashboard polls
#'   snapshot files written by the queue process.
#'
#' Parameters:
#' - `open_viewer`: Whether to open the dashboard URL in the configured viewer.
#' - `announce`: Whether to print the dashboard URL in the console.
#' - `focus_existing`: Whether to reopen an already-running dashboard in the
#'   viewer.
#'
#' Returns:
#' - Invisibly returns the dashboard URL.
#'
#' Assumptions and side effects:
#' - Starts a `callr` background process.
#' - Starts the dashboard control server if needed.
#' - Stores process and URL state in `pkg_env`.
#'
#' @keywords internal
launch_dashboard_background <- function(open_viewer = TRUE, announce = TRUE, focus_existing = FALSE) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("`callr` is required for non-blocking dashboard launch.")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("`shiny` is required for dashboard launch.")
  }

  existing <- pkg_env$dashboard_process %||% NULL
  control_url <- ensure_control_server()
  control_token <- control_server_token()
  cancel_dir <- dashboard_cancel_dir()
  if (dashboard_process_is_alive(existing)) {
    url <- pkg_env$dashboard_url %||% ""
    old_control_url <- pkg_env$dashboard_control_url %||% ""
    if (nzchar(url) && identical(old_control_url, control_url)) {
      if (isTRUE(announce)) {
        cat(sprintf("\nDashboard available at: %s\n", url))
      }
      if (isTRUE(focus_existing)) {
        dashboard_open_viewer(url, open_viewer = open_viewer)
      }
      return(invisible(url))
    }
    stop_dashboard_background()
  }

  snapshot_path <- dashboard_snapshot_path()
  if (!file.exists(snapshot_path)) {
    write_dashboard_snapshot()
  }

  port <- dashboard_pick_port()
  url <- sprintf("http://127.0.0.1:%d", port)
  pkg_path <- dashboard_detect_pkg_path()
  use_pkgload <- nzchar(pkg_path) && file.exists(file.path(pkg_path, "DESCRIPTION"))

  proc <- callr::r_bg(
    func = function(port, pkg_path, use_pkgload, snapshot_path, control_url, control_token, cancel_dir) {
      options(shiny.launch.browser = FALSE)

      if (isTRUE(use_pkgload) &&
          requireNamespace("pkgload", quietly = TRUE) &&
          file.exists(file.path(pkg_path, "DESCRIPTION"))) {
        pkgload::load_all(pkg_path, export_all = FALSE, quiet = TRUE)
      } else {
        if (requireNamespace("taskr", quietly = TRUE)) {
          library(taskr)
        } else {
          stop(
            "Cannot start dashboard background process: `taskr` is not installed ",
            "and no package source path was detected for pkgload::load_all()."
          )
        }
      }

      taskr_ns <- asNamespace("taskr")
      if (!exists("queue_dashboard_app", envir = taskr_ns, inherits = FALSE)) {
        stop(
          "Cannot start dashboard background process: the loaded `taskr` namespace ",
          "is incomplete. Internal dashboard function `queue_dashboard_app()` is ",
          "missing. Restart R, reinstall or reload `taskr`, then try ",
          "`library(taskr); launch_dashboard()` again.",
          call. = FALSE
        )
      }
      app_factory <- get("queue_dashboard_app", envir = taskr_ns, inherits = FALSE)
      shiny::runApp(
        app_factory(
          data_mode = "snapshot",
          snapshot_path = snapshot_path,
          control_url = control_url,
          control_token = control_token,
          cancel_dir = cancel_dir
        ),
        host = "127.0.0.1",
        port = as.integer(port),
        launch.browser = FALSE,
        quiet = TRUE
      )
    },
    args = list(
      port = port,
      pkg_path = pkg_path,
      use_pkgload = use_pkgload,
      snapshot_path = snapshot_path,
      control_url = control_url,
      control_token = control_token,
      cancel_dir = cancel_dir
    ),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )

  pkg_env$dashboard_process <- proc
  pkg_env$dashboard_url <- url
  pkg_env$dashboard_port <- port
  pkg_env$dashboard_control_url <- control_url

  ready <- dashboard_port_ready(port = port, timeout_sec = 6)
  if (!isTRUE(ready)) {
    logs <- dashboard_collect_process_logs(proc)
    if (!dashboard_process_is_alive(proc)) {
      msg <- c(
        "Dashboard failed to start.",
        if (length(logs$stdout) > 0) paste("stdout:", paste(logs$stdout, collapse = "\n")) else NULL,
        if (length(logs$stderr) > 0) paste("stderr:", paste(logs$stderr, collapse = "\n")) else NULL
      )
      warning(paste(msg, collapse = "\n"))
    } else {
      warning("Dashboard started but did not become reachable at ", url, " within 6 seconds.")
    }
    return(invisible(url))
  }

  if (isTRUE(announce)) {
    cat(sprintf("\nDashboard available at: %s\n", url))
  }
  dashboard_open_viewer(url, open_viewer = open_viewer)
  invisible(url)
}

maybe_auto_launch_dashboard <- function() {
  if (!interactive()) {
    return(invisible(NULL))
  }
  # Do not auto-launch dashboard during automated tests.
  if (identical(Sys.getenv("TESTTHAT"), "true") || identical(Sys.getenv("TESTTHAT"), "TRUE")) {
    return(invisible(NULL))
  }
  if (isTRUE(getOption("taskr.testing", FALSE))) {
    return(invisible(NULL))
  }
  if (!isTRUE(getOption("taskr.auto_dashboard", TRUE))) {
    return(invisible(NULL))
  }
  if (!requireNamespace("callr", quietly = TRUE) || !requireNamespace("shiny", quietly = TRUE)) {
    return(invisible(NULL))
  }

  already_alive <- dashboard_process_is_alive(pkg_env$dashboard_process %||% NULL)
  try(
    launch_dashboard_background(
      open_viewer = TRUE,
      announce = !isTRUE(already_alive),
      focus_existing = FALSE
    ),
    silent = TRUE
  )
  invisible(NULL)
}
