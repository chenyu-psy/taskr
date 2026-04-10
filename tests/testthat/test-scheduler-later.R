make_later_scripted_task <- function(status_seq = c("running", "done")) {
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

test_that("start_scheduler_internal schedules when work exists", {
  skip_if_not_installed("later")
  start_scheduler_internal <- getFromNamespace("start_scheduler_internal", "taskr")
  stop_scheduler_internal <- getFromNamespace("stop_scheduler_internal", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$queue <- list(
    list(
      id = "task_later_1",
      label = "later_1",
      resources = list(slots = 1L),
      priority = 0L,
      submit_time = Sys.time(),
      status = "queued",
      output = "none",
      start_task = function(item) make_later_scripted_task(c("running", "done"))
    )
  )

  started <- start_scheduler_internal(interval = 0.01)
  expect_true(started)
  expect_true(is.function(pkg_env$scheduler$scheduler_handle))

  wait_for_later_idle(timeout = 0.5)
  expect_true("task_later_1" %in% names(pkg_env$scheduler$done))
  expect_null(pkg_env$scheduler$scheduler_handle)

  stop_scheduler_internal()
})

test_that("start_scheduler_internal does not start when no work", {
  skip_if_not_installed("later")
  start_scheduler_internal <- getFromNamespace("start_scheduler_internal", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  expect_false(start_scheduler_internal(interval = 0.01))
})

test_that("stop_scheduler_internal clears scheduled handle", {
  skip_if_not_installed("later")
  start_scheduler_internal <- getFromNamespace("start_scheduler_internal", "taskr")
  stop_scheduler_internal <- getFromNamespace("stop_scheduler_internal", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
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

  start_scheduler_internal(interval = 0.1)
  expect_true(is.function(pkg_env$scheduler$scheduler_handle))

  stop_scheduler_internal()
  expect_null(pkg_env$scheduler$scheduler_handle)
})
