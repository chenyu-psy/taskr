make_later_scripted_task <- function(status_seq = c("running", "completed")) {
  state <- new.env(parent = emptyenv())
  state$i <- 1L

  list(
    error = NULL,
    status = function() {
      idx <- min(state$i, length(status_seq))
      out <- status_seq[[idx]]
      state$i <- state$i + 1L
      out
    },
    progress = function() NULL,
    read_output = function() "",
    read_error = function() "",
    is_alive = function() TRUE,
    kill = function() invisible(NULL),
    elapsed = function() 0
  )
}

wait_for_later_idle <- function(timeout = 5) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    later::run_now(0.05)
    Sys.sleep(0.01)
  }
}

test_that("start_scheduler schedules when work exists", {
  skip_if_not_installed("later")
  start_scheduler <- getFromNamespace("start_scheduler", "taskr")
  stop_scheduler <- getFromNamespace("stop_scheduler", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$queue <- list(
    list(
      id = "task_later_1",
      label = "later_1",
      resources = list(slots = 1L),
      priority = 0L,
      submit_time = Sys.time(),
      status = "queued",
      output = "none",
      start_task = function(item) make_later_scripted_task(c("running", "completed"))
    )
  )

  started <- start_scheduler(interval = 0.01)
  expect_true(started)
  expect_true(is.function(pkg_env$scheduler$scheduler_handle))

  wait_for_later_idle(timeout = 0.5)
  expect_true("task_later_1" %in% names(pkg_env$scheduler$finished))
  expect_null(pkg_env$scheduler$scheduler_handle)

  stop_scheduler()
})

test_that("start_scheduler does not start when no work", {
  skip_if_not_installed("later")
  start_scheduler <- getFromNamespace("start_scheduler", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  expect_false(start_scheduler(interval = 0.01))
})

test_that("start_scheduler polls commands while dashboard is alive", {
  skip_if_not_installed("later")
  start_scheduler <- getFromNamespace("start_scheduler", "taskr")
  stop_scheduler <- getFromNamespace("stop_scheduler", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  old_dashboard_process <- pkg_env$dashboard_process
  pkg_env$dashboard_process <- list(is_alive = function() TRUE)
  on.exit({
    pkg_env$dashboard_process <- old_dashboard_process
    taskr::shutdown_queue()
  }, add = TRUE)

  expect_true(start_scheduler(interval = 0.01))
  expect_true(is.function(pkg_env$scheduler$scheduler_handle))

  stop_scheduler()
})

test_that("stop_scheduler clears scheduled handle", {
  skip_if_not_installed("later")
  start_scheduler <- getFromNamespace("start_scheduler", "taskr")
  stop_scheduler <- getFromNamespace("stop_scheduler", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$queue <- list(
    list(
      id = "task_later_2",
      label = "later_2",
      resources = list(slots = 1L),
      priority = 0L,
      submit_time = Sys.time(),
      status = "queued",
      output = "none",
      start_task = function(item) make_later_scripted_task(c("running", "running"))
    )
  )

  start_scheduler(interval = 0.1)
  expect_true(is.function(pkg_env$scheduler$scheduler_handle))

  stop_scheduler()
  expect_null(pkg_env$scheduler$scheduler_handle)
})
