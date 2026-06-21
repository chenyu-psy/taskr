make_killable_running_task <- function() {
  state <- new.env(parent = emptyenv())
  state$cancelled <- FALSE

  list(
    error = NULL,
    status = function() "running",
    progress = function() NULL,
    read_output = function() "",
    read_error = function() "",
    is_alive = function() !state$cancelled,
    kill = function() {
      state$cancelled <- TRUE
      invisible(NULL)
    },
    elapsed = function() 0,
    state = state
  )
}

make_control_item <- function(id, label, status, path = NULL, task = NULL) {
  list(
    id = id,
    label = label,
    status = status,
    priority = 0L,
    resources = list(slots = 1L),
    output = "all",
    submit_time = Sys.time(),
    start_time = Sys.time(),
    end_time = as.POSIXct(NA),
    result_path = path,
    task = task,
    error = NULL
  )
}

test_that("cancel_task removes pending task by numeric id and marks it cancelled", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  path <- tempfile(fileext = ".rds")
  saveRDS(1L, path)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$pending <- list(make_control_item(1L, "repeat", "pending", path = path))

  taskr::cancel_task(1)

  expect_length(pkg_env$scheduler$pending, 0)
  expect_true("1" %in% names(pkg_env$scheduler$finished))
  expect_identical(pkg_env$scheduler$finished[["1"]]$status, "cancelled")
  expect_false(file.exists(path))
})

test_that("cancel_task kills running tasks and supports vector ids", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  task_1 <- make_killable_running_task()
  task_2 <- make_killable_running_task()
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$running <- list(
    "1" = make_control_item(1L, "same_name", "running", task = task_1),
    "2" = make_control_item(2L, "same_name", "running", task = task_2)
  )

  taskr::cancel_task(c(1, 2))

  expect_true(task_1$state$cancelled)
  expect_true(task_2$state$cancelled)
  expect_length(pkg_env$scheduler$running, 0)
  expect_setequal(names(pkg_env$scheduler$finished), c("1", "2"))
})

test_that("cancel_task does not use labels as task references", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list("1" = make_control_item(1L, "model_a", "completed"))

  expect_error(taskr::cancel_task("model_a"), "positive integer")
})

test_that("cancel_task validates missing and terminal tasks", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list("1" = make_control_item(1L, "done", "completed"))

  expect_warning(taskr::cancel_task(1), "already terminal")
  expect_error(taskr::cancel_task(99), "Task not found")
  expect_error(taskr::cancel_task(1.5), "positive integer")
})

test_that("remove_task removes finished tasks and supports vector ids", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  p1 <- tempfile(fileext = ".rds")
  p2 <- tempfile(fileext = ".rds")
  saveRDS(1L, p1)
  saveRDS(2L, p2)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    "1" = make_control_item(1L, "same_name", "completed", path = p1),
    "2" = make_control_item(2L, "same_name", "failed", path = p2),
    "3" = make_control_item(3L, "keep", "cancelled")
  )

  taskr::remove_task(c(1, 2))

  expect_false("1" %in% names(pkg_env$scheduler$finished))
  expect_false("2" %in% names(pkg_env$scheduler$finished))
  expect_true("3" %in% names(pkg_env$scheduler$finished))
  expect_false(file.exists(p1))
  expect_false(file.exists(p2))
})

test_that("remove_task cancels active work before removing its record", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  task_obj <- make_killable_running_task()
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$running <- list("1" = make_control_item(1L, "running_remove", "running", task = task_obj))

  taskr::remove_task(1)

  expect_true(task_obj$state$cancelled)
  expect_false("1" %in% names(pkg_env$scheduler$running))
  expect_false("1" %in% names(pkg_env$scheduler$finished))
})

test_that("clean_tasks removes completed records and deletes result files", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  p1 <- tempfile(fileext = ".rds")
  p2 <- tempfile(fileext = ".rds")
  saveRDS(1L, p1)
  saveRDS(2L, p2)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    "1" = make_control_item(1L, "a", "completed", path = p1),
    "2" = make_control_item(2L, "b", "failed", path = p2),
    "3" = make_control_item(3L, "c", "cancelled", path = NULL)
  )

  taskr::clean_tasks()

  expect_length(pkg_env$scheduler$finished, 0)
  expect_false(file.exists(p1))
  expect_false(file.exists(p2))
})
