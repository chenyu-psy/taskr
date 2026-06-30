make_kill_tracking_task <- function(id = 1L) {
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

make_running_task <- function() {
  list(
    read_output = function() "",
    read_error = function() "",
    progress = function() NULL,
    status = function() "running",
    stdout_buffer = "",
    stderr_buffer = ""
  )
}

test_that("init_queue creates scheduler state with requested capacity", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 3)

  expect_true(!is.null(pkg_env$scheduler))
  expect_equal(pkg_env$scheduler$capacity$slots, 3L)
  expect_equal(pkg_env$scheduler$next_id, 1L)
  expect_length(pkg_env$scheduler$pending, 0)
  expect_length(pkg_env$scheduler$running, 0)
  expect_length(pkg_env$scheduler$finished, 0)
})

test_that("init_queue validates max_slots", {
  expect_error(taskr::init_queue(max_slots = 0), "must be >= 1")
  expect_error(taskr::init_queue(max_slots = NA_real_), "single positive integer")
  expect_error(taskr::init_queue(max_slots = c(1, 2)), "single positive integer")
})

test_that("set_queue updates capacity without resetting tasks", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")

  taskr::init_queue(max_slots = 3)
  pkg_env$scheduler$next_id <- 9L
  pkg_env$scheduler$pending <- list(list(
    id = 2L,
    status = "pending",
    priority = 0L,
    submit_time = Sys.time(),
    resources = list(slots = 3L),
    start_task = function(item) stop("should not launch")
  ))
  pkg_env$scheduler$running <- list("1" = list(
    id = 1L,
    status = "running",
    resources = list(slots = 2L),
    task = make_running_task()
  ))
  pkg_env$scheduler$finished <- list("3" = list(
    id = 3L,
    status = "completed",
    resources = list(slots = 1L)
  ))

  taskr::set_queue(max_slots = 1)

  expect_equal(pkg_env$scheduler$capacity$slots, 1L)
  expect_equal(pkg_env$scheduler$next_id, 9L)
  expect_true("1" %in% names(pkg_env$scheduler$running))
  expect_length(pkg_env$scheduler$pending, 1)
  expect_true("3" %in% names(pkg_env$scheduler$finished))
})

test_that("set_queue initializes queue and validates max_slots", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  read_dashboard_snapshot <- getFromNamespace("read_dashboard_snapshot", "taskr")

  taskr::shutdown_queue()
  taskr::set_queue(max_slots = 3)

  expect_true(!is.null(pkg_env$scheduler))
  expect_equal(pkg_env$scheduler$capacity$slots, 3L)
  expect_equal(read_dashboard_snapshot()$max_slots, 3L)

  expect_error(taskr::set_queue(max_slots = 0), "must be >= 1")
  expect_error(taskr::set_queue(max_slots = NA_real_), "single positive integer")
  expect_error(taskr::set_queue(max_slots = c(1, 2)), "single positive integer")
})

test_that("shutdown_queue clears scheduler state and temp files", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  register_active_task <- getFromNamespace("register_active_task", "taskr")
  task_tmpfile <- getFromNamespace("task_tmpfile", "taskr")

  taskr::init_queue(max_slots = 1)

  fake_task <- make_kill_tracking_task(1L)
  pkg_env$scheduler$running <- list(
    "1" = list(
      id = 1L,
      resources = list(slots = 1L),
      task = fake_task
    )
  )
  register_active_task(fake_task)

  path <- task_tmpfile(1L)
  writeLines("tmp", con = path)
  expect_true(file.exists(path))

  taskr::shutdown_queue()

  expect_true(fake_task$state$cancelled)
  expect_null(pkg_env$scheduler)
  expect_false(file.exists(path))
  expect_length(ls(pkg_env$active_tasks, all.names = TRUE), 0)
})
