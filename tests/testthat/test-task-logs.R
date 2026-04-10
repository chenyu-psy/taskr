make_log_scripted_task <- function(
    status_seq = c("running", "done"),
    stdout = "out\n",
    stderr = "err\n") {
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
    read_output = function() stdout,
    read_error = function() stderr,
    is_alive = function() TRUE,
    kill = function() invisible(NULL),
    elapsed = function() 0
  )
}

test_that("task_logs returns buffered logs for a done task", {
  task_logs <- getFromNamespace("task_logs", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$done <- list(
    task_001 = list(
      id = "task_001",
      label = "log_done",
      status = "done",
      stdout_buffer = "hello\n",
      stderr_buffer = ""
    )
  )

  out <- task_logs("log_done")
  expect_identical(out$id, "task_001")
  expect_identical(out$status, "done")
  expect_identical(out$stdout, "hello\n")
})

test_that("task_logs recycles running logs before returning", {
  task_logs <- getFromNamespace("task_logs", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$running <- list(
    task_010 = list(
      id = "task_010",
      label = "log_run",
      status = "running",
      resources = list(slots = 1L),
      stdout_buffer = "",
      stderr_buffer = "",
      task = make_log_scripted_task(
        status_seq = c("running", "running"),
        stdout = "chunk\n",
        stderr = "warn\n"
      )
    )
  )

  out <- task_logs("log_run")
  expect_identical(out$status, "running")
  expect_true(grepl("chunk", out$stdout))
  expect_true(grepl("warn", out$stderr))
})

test_that("task_logs errors when task is missing", {
  task_logs <- getFromNamespace("task_logs", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  expect_error(task_logs("missing"), "Task not found")
})
