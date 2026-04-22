make_log_scripted_task <- function(
    status_seq = c("running", "completed"),
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

test_that("get_task_log returns buffered logs for a completed task", {
  get_task_log <- getFromNamespace("get_task_log", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    task_001 = list(
      id = "task_001",
      label = "log_done",
      status = "completed",
      stdout_buffer = "hello\n",
      stderr_buffer = ""
    )
  )

  out <- get_task_log("log_done")
  expect_identical(out$id, "task_001")
  expect_identical(out$status, "completed")
  expect_identical(out$stdout, "hello\n")
})

test_that("get_task_log recycles running logs before returning", {
  get_task_log <- getFromNamespace("get_task_log", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
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

  out <- get_task_log("log_run")
  expect_identical(out$status, "running")
  expect_true(grepl("chunk", out$stdout))
  expect_true(grepl("warn", out$stderr))
})

test_that("get_task_log errors when task is missing", {
  get_task_log <- getFromNamespace("get_task_log", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  expect_error(get_task_log("missing"), "Task not found")
})
