test_that(".onLoad creates a package temp directory", {
  expect_true(dir.exists(taskr:::pkg_env$tempdir))
  expect_match(basename(taskr:::pkg_env$tempdir), "^taskr$")
})

test_that("task_tmpfile builds a stable result path from the task id", {
  path <- taskr:::task_tmpfile("task_001")

  expect_match(path, "task_001[.]rds$")
  expect_identical(dirname(path), taskr:::pkg_env$tempdir)
})

test_that("task_tmpfile validates the id input", {
  expect_error(taskr:::task_tmpfile(""), "single non-empty character string")
  expect_error(taskr:::task_tmpfile(NA_character_), "single non-empty character string")
})

test_that("register_active_task and unregister_active_task update package state", {
  task <- taskr:::Task$new(id = "task_state_001")

  taskr:::register_active_task(task)
  expect_true(exists("task_state_001", envir = taskr:::pkg_env$active_tasks, inherits = FALSE))

  taskr:::unregister_active_task("task_state_001")
  expect_false(exists("task_state_001", envir = taskr:::pkg_env$active_tasks, inherits = FALSE))
})

test_that("cleanup_active_tasks kills tracked tasks", {
  fake_process <- list(
    is_alive = function() TRUE,
    kill = function() invisible(NULL)
  )

  task <- taskr:::Task$new(
    id = "task_state_002",
    process = fake_process,
    status = "running"
  )

  taskr:::register_active_task(task)
  taskr:::cleanup_active_tasks()

  expect_equal(task$status(), "killed")
  expect_false(exists("task_state_002", envir = taskr:::pkg_env$active_tasks, inherits = FALSE))
})

test_that("read_task_result_file returns stored results", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(list(value = 42), path)

  result <- taskr:::read_task_result_file(path)

  expect_equal(result$value, 42)
})

test_that("read_task_result_file reports a clear missing-file error", {
  path <- tempfile(fileext = ".rds")

  expect_error(
    taskr:::read_task_result_file(path),
    "Task result file is missing"
  )
})

test_that(".onUnload kills tracked tasks and removes the temp directory", {
  tempdir_path <- taskr:::pkg_env$tempdir
  fake_process <- list(
    is_alive = function() TRUE,
    kill = function() invisible(NULL)
  )
  task <- taskr:::Task$new(
    id = "task_state_003",
    process = fake_process,
    status = "running"
  )

  taskr:::register_active_task(task)
  taskr:::.onUnload("")

  expect_equal(task$status(), "killed")
  expect_false(dir.exists(tempdir_path))

  taskr:::.onLoad("", "taskr")
  expect_true(dir.exists(taskr:::pkg_env$tempdir))
})

test_that(".onUnload stops scheduler handle and clears scheduler state", {
  on_load <- getFromNamespace(".onLoad", "taskr")
  on_unload <- getFromNamespace(".onUnload", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")

  on_load("", "taskr")
  on.exit(on_load("", "taskr"), add = TRUE)

  canceled <- FALSE
  pkg_env$scheduler <- list(
    scheduler_handle = function() {
      canceled <<- TRUE
      invisible(NULL)
    }
  )

  on_unload("")

  expect_true(canceled)
  expect_null(pkg_env$scheduler)
})
