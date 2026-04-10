make_result_running_task <- function(status_seq = c("running", "done")) {
  state <- new.env(parent = emptyenv())
  state$idx <- 1L

  list(
    error = NULL,
    status = function() {
      i <- min(state$idx, length(status_seq))
      value <- status_seq[[i]]
      state$idx <- state$idx + 1L
      value
    },
    progress = function() NULL,
    read_output = function() "",
    read_error = function() "",
    is_alive = function() TRUE,
    kill = function() invisible(NULL),
    elapsed = function() 0
  )
}

test_that("task_result returns done task result by id", {
  task_result <- getFromNamespace("task_result", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  path <- tempfile(fileext = ".rds")
  saveRDS(list(value = 7L), path)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$done <- list(
    task_001 = list(
      id = "task_001",
      label = "fit_a",
      status = "done",
      output = "all",
      result_path = path
    )
  )

  out <- task_result("task_001")
  expect_equal(out$value, 7L)
})

test_that("task_result can select by label", {
  task_result <- getFromNamespace("task_result", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  path <- tempfile(fileext = ".rds")
  saveRDS(11L, path)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$done <- list(
    task_010 = list(
      id = "task_010",
      label = "label_a",
      status = "done",
      output = "all",
      result_path = path
    )
  )

  expect_equal(task_result("label_a"), 11L)
})

test_that("task_result warns and returns NULL when output is none", {
  task_result <- getFromNamespace("task_result", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$done <- list(
    task_011 = list(
      id = "task_011",
      label = "side_effect",
      status = "done",
      output = "none",
      result_path = NULL
    )
  )

  expect_warning(
    expect_null(task_result("task_011")),
    "output = \"none\""
  )
})

test_that("task_result errors clearly for failed and killed tasks", {
  task_result <- getFromNamespace("task_result", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$done <- list(
    task_020 = list(id = "task_020", label = "bad", status = "failed", error = "boom"),
    task_021 = list(id = "task_021", label = "killed", status = "killed")
  )

  expect_error(task_result("task_020"), "boom")
  expect_error(task_result("task_021"), "killed")
})

test_that("task_result waits for running task to finish", {
  task_result <- getFromNamespace("task_result", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  path <- tempfile(fileext = ".rds")
  saveRDS("done_later", path)

  running_item <- list(
    id = "task_030",
    label = "run_then_done",
    status = "running",
    output = "all",
    result_path = path,
    resources = list(slots = 1L),
    task = make_result_running_task(status_seq = c("running", "done")),
    submit_time = Sys.time(),
    start_time = Sys.time(),
    end_time = as.POSIXct(NA)
  )

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$running <- list(task_030 = running_item)

  expect_identical(task_result("task_030"), "done_later")
})

test_that("task_result reports missing tasks and ambiguous labels", {
  task_result <- getFromNamespace("task_result", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$done <- list(
    task_100 = list(id = "task_100", label = "dup", status = "done", output = "none"),
    task_101 = list(id = "task_101", label = "dup", status = "done", output = "none")
  )

  expect_error(task_result("missing"), "Task not found")
  expect_error(task_result("dup"), "More than one task matches")
})
