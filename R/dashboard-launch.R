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

dashboard_detect_pkg_path <- function() {
  wd_path <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(wd_path, "DESCRIPTION"))) {
    return(wd_path)
  }

  ns_path <- tryCatch(getNamespaceInfo("taskr", "path"), error = function(e) "")
  if (is.character(ns_path) && nzchar(ns_path)) {
    ns_path <- normalizePath(ns_path, winslash = "/", mustWork = FALSE)
    if (file.exists(file.path(ns_path, "DESCRIPTION"))) {
      return(ns_path)
    }
    parent_path <- dirname(ns_path)
    if (file.exists(file.path(parent_path, "DESCRIPTION"))) {
      return(parent_path)
    }
  }

  ""
}

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

launch_dashboard_background <- function(open_viewer = TRUE, announce = TRUE, focus_existing = FALSE) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("`callr` is required for non-blocking dashboard launch.")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("`shiny` is required for dashboard launch.")
  }

  existing <- pkg_env$dashboard_process %||% NULL
  if (dashboard_process_is_alive(existing)) {
      url <- pkg_env$dashboard_url %||% ""
      if (nzchar(url)) {
      if (isTRUE(announce)) {
        cat(sprintf("\nDashboard available at: %s\n", url))
      }
      if (isTRUE(focus_existing)) {
        dashboard_open_viewer(url, open_viewer = open_viewer)
      }
      return(invisible(url))
    }
  }

  snapshot_path <- dashboard_snapshot_path()
  command_path <- dashboard_command_path()
  if (!file.exists(snapshot_path)) {
    write_dashboard_snapshot()
  }
  ensure_dashboard_command_file(command_path)

  port <- dashboard_pick_port()
  url <- sprintf("http://127.0.0.1:%d", port)
  pkg_path <- dashboard_detect_pkg_path()
  use_pkgload <- nzchar(pkg_path) && file.exists(file.path(pkg_path, "DESCRIPTION"))

  proc <- callr::r_bg(
    func = function(port, pkg_path, use_pkgload, snapshot_path, command_path) {
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

      app_factory <- getFromNamespace("queue_dashboard_app", "taskr")
      shiny::runApp(
        app_factory(data_mode = "snapshot", snapshot_path = snapshot_path, command_path = command_path),
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
      command_path = command_path
    ),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )

  pkg_env$dashboard_process <- proc
  pkg_env$dashboard_url <- url
  pkg_env$dashboard_port <- port

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
      warning("Dashboard started but fid not become reachable at ", url, " within 6 seconds.")
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
