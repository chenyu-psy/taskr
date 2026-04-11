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

test_that("cancel_task removes queued task and marks it cancelled", {
  cancel_task <- getFromNamespace("cancel_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  path <- tempfile(fileext = ".rds")
  saveRDS(1L, path)

  q <- make_control_item("task_001", "queued_a", "queued", path = path)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$queue <- list(q)

  cancel_task("task_001")

  expect_length(pkg_env$scheduler$queue, 0)
  expect_true("task_001" %in% names(pkg_env$scheduler$done))
  expect_identical(pkg_env$scheduler$done$task_001$status, "cancelled")
  expect_false(file.exists(path))
})

test_that("cancel_task kills a running task", {
  cancel_task <- getFromNamespace("cancel_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  task_obj <- make_killable_running_task()
  r <- make_control_item("task_010", "run_a", "running", task = task_obj)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$running <- list(task_010 = r)

  cancel_task("run_a")

  expect_true(task_obj$state$cancelled)
  expect_length(pkg_env$scheduler$running, 0)
  expect_identical(pkg_env$scheduler$done$task_010$status, "cancelled")
})

test_that("cancel_task warns when task is already terminal", {
  cancel_task <- getFromNamespace("cancel_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  d <- make_control_item("task_020", "done_a", "done")
  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$done <- list(task_020 = d)

  expect_warning(cancel_task("task_020"), "already terminal")
})

test_that("cancel_task validates lookup behavior", {
  cancel_task <- getFromNamespace("cancel_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$done <- list(
    task_030 = make_control_item("task_030", "dup", "done"),
    task_031 = make_control_item("task_031", "dup", "done")
  )

  expect_error(cancel_task("missing"), "Task not found")
  expect_error(cancel_task("dup"), "More than one task matches")
})

test_that("clean_tasks removes done records and deletes result files", {
  clean_tasks <- getFromNamespace("clean_tasks", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  p1 <- tempfile(fileext = ".rds")
  p2 <- tempfile(fileext = ".rds")
  saveRDS(1L, p1)
  saveRDS(2L, p2)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 1)
  pkg_env$scheduler$done <- list(
    task_100 = make_control_item("task_100", "a", "done", path = p1),
    task_101 = make_control_item("task_101", "b", "failed", path = p2),
    task_102 = make_control_item("task_102", "c", "cancelled", path = NULL)
  )
  pkg_env$scheduler$label_index <- list(
    a = "task_100",
    b = "task_101",
    c = "task_102"
  )

  clean_tasks()

  expect_length(pkg_env$scheduler$done, 0)
  expect_false(file.exists(p1))
  expect_false(file.exists(p2))
  expect_null(pkg_env$scheduler$label_index[["a"]])
  expect_null(pkg_env$scheduler$label_index[["b"]])
  expect_null(pkg_env$scheduler$label_index[["c"]])
})
