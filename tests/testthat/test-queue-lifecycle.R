make_kill_tracking_task <- function(id = "task_run") {
  state <- new.env(parent = emptyenv())
  state$cancelled <- FALSE

  list(
    id = id,
    kill = function() {
      state$cancelled <- TRUE
      invisible(NULL)
    },
    state = state
  )
}

test_that("init_queue creates scheduler state with requested capacity", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 3)

  expect_true(!is.null(pkg_env$scheduler))
  expect_equal(pkg_env$scheduler$capacity$slots, 3L)
  expect_equal(pkg_env$scheduler$next_id, 1L)
  expect_length(pkg_env$scheduler$queue, 0)
  expect_length(pkg_env$scheduler$running, 0)
  expect_length(pkg_env$scheduler$finished, 0)
})

test_that("init_queue validates max_slots", {
  expect_error(taskr::init_queue(max_slots = 0), "must be >= 1")
  expect_error(taskr::init_queue(max_slots = NA_real_), "single positive integer")
  expect_error(taskr::init_queue(max_slots = c(1, 2)), "single positive integer")
})

test_that("shutdown_queue clears scheduler state and temp files", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  register_active_task <- getFromNamespace("register_active_task", "taskr")
  task_tmpfile <- getFromNamespace("task_tmpfile", "taskr")

  taskr::init_queue(max_slots = 1)

  fake_task <- make_kill_tracking_task("task_001")
  pkg_env$scheduler$running <- list(
    task_001 = list(
      id = "task_001",
      resources = list(slots = 1L),
      task = fake_task
    )
  )
  register_active_task(fake_task)

  path <- task_tmpfile("task_001")
  writeLines("tmp", con = path)
  expect_true(file.exists(path))

  taskr::shutdown_queue()

  expect_true(fake_task$state$cancelled)
  expect_null(pkg_env$scheduler)
  expect_false(file.exists(path))
  expect_length(ls(pkg_env$active_tasks, all.names = TRUE), 0)
})
