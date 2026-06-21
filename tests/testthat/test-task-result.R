make_result_running_task <- function(status_seq = c("running", "completed")) {
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

test_that("get_task_result returns completed task result by numeric id", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  path <- tempfile(fileext = ".rds")
  saveRDS(list(value = 7L), path)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    "1" = list(id = 1L, label = "fit_a", status = "completed", output = "all", result_path = path)
  )

  out <- taskr::get_task_result(1)
  expect_equal(out$value, 7L)
  expect_equal(taskr::get_task_result("1")$value, 7L)
})

test_that("get_task_result does not select by label", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  path <- tempfile(fileext = ".rds")
  saveRDS(11L, path)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    "1" = list(id = 1L, label = "label_a", status = "completed", output = "all", result_path = path)
  )

  expect_error(taskr::get_task_result("label_a"), "positive integer")
})

test_that("get_task_result warns and returns NULL when output is none", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    "1" = list(id = 1L, label = "side_effect", status = "completed", output = "none", result_path = NULL)
  )

  expect_warning(expect_null(taskr::get_task_result(1)), "output = \"none\"")
})

test_that("get_task_result errors clearly for failed and cancelled tasks", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    "1" = list(id = 1L, label = "bad", status = "failed", error = "boom"),
    "2" = list(id = 2L, label = "cancelled", status = "cancelled")
  )

  expect_error(taskr::get_task_result(1), "boom")
  expect_error(taskr::get_task_result(2), "cancelled")
})

test_that("get_task_result waits for running task to finish", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  path <- tempfile(fileext = ".rds")
  saveRDS("done_later", path)

  running_item <- list(
    id = 1L,
    label = "run_then_done",
    status = "running",
    output = "all",
    result_path = path,
    resources = list(slots = 1L),
    task = make_result_running_task(status_seq = c("running", "completed")),
    submit_time = Sys.time(),
    start_time = Sys.time(),
    end_time = as.POSIXct(NA)
  )

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$running <- list("1" = running_item)

  expect_identical(taskr::get_task_result(1), "done_later")
})

test_that("get_task_result reports missing and invalid ids", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  expect_error(taskr::get_task_result(99), "Task not found")
  expect_error(taskr::get_task_result(c(1, 2)), "single positive integer")
})
