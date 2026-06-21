# Later-Based Scheduler Loop (Internal)
#
# Purpose:
# - Provide lazy start/stop of background scheduler ticks.
# - Keep scheduling concerns separate from submission/query APIs.

validate_scheduler_interval <- function(interval) {
  if (!is.numeric(interval) || length(interval) != 1 || is.na(interval) || interval <= 0) {
    stop("`interval` must be a single positive numeric value.")
  }

  as.numeric(interval)
}

scheduler_has_work <- function(state) {
  if (is.null(state)) {
    return(FALSE)
  }

  length(state$pending %||% list()) > 0 || length(state$running %||% list()) > 0
}

schedule_next_tick <- function() {
  if (is.null(pkg_env$scheduler)) {
    return(invisible(NULL))
  }

  interval <- as.numeric(pkg_env$scheduler$scheduler_interval %||% 1.0)
  pkg_env$scheduler$scheduler_handle <- later::later(
    func = update_scheduler,
    delay = interval
  )

  invisible(NULL)
}

update_scheduler <- function() {
  if (is.null(pkg_env$scheduler)) {
    return(invisible(NULL))
  }

  pkg_env$scheduler$scheduler_handle <- NULL
  pkg_env$scheduler <- update_queue(pkg_env$scheduler)
  write_dashboard_snapshot()

  if (!scheduler_has_work(pkg_env$scheduler) || isTRUE(pkg_env$scheduler$scheduler_should_stop)) {
    stop_scheduler()
    return(invisible(NULL))
  }

  schedule_next_tick()
  invisible(NULL)
}

start_scheduler <- function(interval = NULL) {
  if (is.null(pkg_env$scheduler)) {
    return(invisible(FALSE))
  }

  if (!is.null(interval)) {
    pkg_env$scheduler$scheduler_interval <- validate_scheduler_interval(interval)
  }

  if (!scheduler_has_work(pkg_env$scheduler)) {
    return(invisible(FALSE))
  }

  if (is.function(pkg_env$scheduler$scheduler_handle %||% NULL)) {
    return(invisible(FALSE))
  }

  schedule_next_tick()
  invisible(TRUE)
}

stop_scheduler <- function() {
  if (is.null(pkg_env$scheduler)) {
    return(invisible(FALSE))
  }

  handle <- pkg_env$scheduler$scheduler_handle %||% NULL
  if (is.function(handle)) {
    try(handle(), silent = TRUE)
  }

  pkg_env$scheduler$scheduler_handle <- NULL
  invisible(TRUE)
}
