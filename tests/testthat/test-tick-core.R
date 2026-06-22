make_tick_state <- function(capacity_slots = 2L, pending = list(), running = list(), finished = list()) {
  list(
    capacity = list(slots = as.integer(capacity_slots)),
    pending = pending,
    running = running,
    finished = finished,
    scheduler_should_stop = FALSE
  )
}

make_scripted_task <- function(
    id,
    status_seq = c("running", "completed"),
    progress = NULL,
    output = "",
    error_output = "",
    error_message = NULL) {
  state <- new.env(parent = emptyenv())
  state$idx <- 1L

  list(
    id = id,
    error = error_message,
    status = function() {
      i <- min(state$idx, length(status_seq))
      value <- status_seq[[i]]
      state$idx <- state$idx + 1L
      value
    },
    progress = function() progress,
    read_output = function() output,
    read_error = function() error_output,
    is_alive = function() TRUE,
    kill = function() invisible(NULL),
    elapsed = function() 0
  )
}

test_that("update_queue starts tasks by priority then FIFO", {
  q1 <- list(id = 1L, priority = 1L, submit_time = 2, resources = list(slots = 1L))
  q2 <- list(id = 2L, priority = 10L, submit_time = 3, resources = list(slots = 1L))
  q3 <- list(id = 3L, priority = 10L, submit_time = 1, resources = list(slots = 1L))

  state <- make_tick_state(capacity_slots = 2L, pending = list(q1, q2, q3))
  next_state <- taskr:::update_queue(
    state = state,
    start_task_fn = function(item) make_scripted_task(id = item$id, status_seq = "running")
  )

  expect_identical(names(next_state$running), c("3", "2"))
  expect_length(next_state$pending, 1)
  expect_identical(next_state$pending[[1]]$id, 1L)
})

test_that("update_queue skips pending tasks with dashboard cancel markers", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  old_cancel_dir <- pkg_env$dashboard_cancel_dir
  on.exit({
    pkg_env$dashboard_cancel_dir <- old_cancel_dir
  }, add = TRUE)

  pkg_env$dashboard_cancel_dir <- tempfile("taskr-cancel-")
  dir.create(pkg_env$dashboard_cancel_dir, recursive = TRUE)

  q1 <- list(id = 1L, priority = 10L, submit_time = 1, resources = list(slots = 1L))
  q2 <- list(id = 2L, priority = 1L, submit_time = 2, resources = list(slots = 1L))
  taskr:::write_dashboard_cancel_marker(1L)

  state <- make_tick_state(capacity_slots = 1L, pending = list(q1, q2))
  next_state <- taskr:::update_queue(
    state = state,
    start_task_fn = function(item) make_scripted_task(id = item$id, status_seq = "running")
  )

  expect_true("1" %in% names(next_state$finished))
  expect_equal(next_state$finished[["1"]]$status, "cancelled")
  expect_true("2" %in% names(next_state$running))
  expect_false(taskr:::dashboard_cancel_requested(1L))
})

test_that("update_queue cancels running tasks with dashboard cancel markers", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  old_cancel_dir <- pkg_env$dashboard_cancel_dir
  on.exit({
    pkg_env$dashboard_cancel_dir <- old_cancel_dir
  }, add = TRUE)

  pkg_env$dashboard_cancel_dir <- tempfile("taskr-cancel-")
  dir.create(pkg_env$dashboard_cancel_dir, recursive = TRUE)

  task <- make_scripted_task(id = 1L, status_seq = "running")
  running_item <- list(
    id = 1L,
    priority = 0L,
    resources = list(slots = 1L),
    status = "running",
    task = task
  )
  taskr:::write_dashboard_cancel_marker(1L)

  state <- make_tick_state(capacity_slots = 1L, running = list("1" = running_item))
  next_state <- taskr:::update_queue(state)

  expect_length(next_state$running, 0)
  expect_true("1" %in% names(next_state$finished))
  expect_equal(next_state$finished[["1"]]$status, "cancelled")
  expect_false(taskr:::dashboard_cancel_requested(1L))
})

test_that("update_queue cleans finished tasks with dashboard cleanup marker", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  old_cancel_dir <- pkg_env$dashboard_cancel_dir
  on.exit({
    pkg_env$dashboard_cancel_dir <- old_cancel_dir
  }, add = TRUE)

  pkg_env$dashboard_cancel_dir <- tempfile("taskr-cancel-")
  dir.create(pkg_env$dashboard_cancel_dir, recursive = TRUE)

  finished_item <- list(
    id = 1L,
    label = "finished_label",
    status = "completed",
    resources = list(slots = 1L),
    result_path = NULL
  )
  state <- make_tick_state(capacity_slots = 1L, finished = list("1" = finished_item))
  taskr:::write_dashboard_clean_finished_marker()

  next_state <- taskr:::update_queue(state)

  expect_length(next_state$finished, 0)
  expect_false(taskr:::dashboard_clean_finished_requested())
})

test_that("update_queue cancels active tasks before dashboard cleanup", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  old_cancel_dir <- pkg_env$dashboard_cancel_dir
  on.exit({
    pkg_env$dashboard_cancel_dir <- old_cancel_dir
  }, add = TRUE)

  pkg_env$dashboard_cancel_dir <- tempfile("taskr-cancel-")
  dir.create(pkg_env$dashboard_cancel_dir, recursive = TRUE)

  pending_item <- list(
    id = 1L,
    label = "pending_label",
    status = "pending",
    priority = 0L,
    submit_time = 1,
    resources = list(slots = 1L),
    result_path = NULL
  )
  old_finished <- list(
    id = 2L,
    label = "old_label",
    status = "completed",
    resources = list(slots = 1L),
    result_path = NULL
  )
  state <- make_tick_state(
    capacity_slots = 1L,
    pending = list(pending_item),
    finished = list("2" = old_finished)
  )
  taskr:::write_dashboard_cancel_marker(1L)
  taskr:::write_dashboard_clean_finished_marker()

  next_state <- taskr:::update_queue(
    state = state,
    start_task_fn = function(item) make_scripted_task(id = item$id, status_seq = "running")
  )

  expect_length(next_state$pending, 0)
  expect_length(next_state$running, 0)
  expect_length(next_state$finished, 0)
  expect_false(taskr:::dashboard_cancel_requested(1L))
  expect_false(taskr:::dashboard_clean_finished_requested())
})

test_that("update_queue recycles terminal running tasks into finished", {
  running_item <- list(
    id = 10L,
    priority = 0L,
    resources = list(slots = 1L),
    status = "running",
    task = make_scripted_task(
      id = "task_010",
      status_seq = "completed",
      progress = list(fraction = 1, message = "completed", updated_at = Sys.time()),
      output = "hello\n"
    )
  )

  state <- make_tick_state(
    capacity_slots = 1L,
    running = list("10" = running_item),
    pending = list()
  )

  next_state <- taskr:::update_queue(state)

  expect_length(next_state$running, 0)
  expect_true("10" %in% names(next_state$finished))
  expect_equal(next_state$finished[["10"]]$status, "completed")
  expect_equal(next_state$finished[["10"]]$stdout_buffer, "hello\n")
  expect_equal(next_state$finished[["10"]]$progress, 1)
})

test_that("update_queue recycles failed and cancelled tasks into finished with metadata", {
  failed_item <- list(
    id = 1L,
    priority = 0L,
    resources = list(slots = 1L),
    status = "running",
    task = make_scripted_task(
      id = 1L,
      status_seq = "failed",
      output = "partial\n",
      error_output = "boom\n",
      error_message = "boom"
    )
  )
  cancelled_item <- list(
    id = 2L,
    priority = 0L,
    resources = list(slots = 1L),
    status = "running",
    task = make_scripted_task(
      id = 2L,
      status_seq = "cancelled",
      output = "",
      error_output = ""
    )
  )

  state <- make_tick_state(
    capacity_slots = 2L,
    running = list("1" = failed_item, "2" = cancelled_item),
    pending = list()
  )

  next_state <- taskr:::update_queue(state)

  expect_length(next_state$running, 0)
  expect_equal(next_state$finished[["1"]]$status, "failed")
  expect_equal(next_state$finished[["1"]]$error, "boom")
  expect_equal(next_state$finished[["1"]]$stderr_buffer, "boom\n")
  expect_equal(next_state$finished[["2"]]$status, "cancelled")
})

test_that("update_queue skips oversized head task and can start later smaller task", {
  big_task <- list(id = 1L, priority = 9L, submit_time = 1, resources = list(slots = 3L))
  small_task <- list(id = 2L, priority = 1L, submit_time = 2, resources = list(slots = 1L))

  state <- make_tick_state(capacity_slots = 2L, pending = list(big_task, small_task))
  next_state <- taskr:::update_queue(
    state = state,
    start_task_fn = function(item) make_scripted_task(id = item$id, status_seq = "running")
  )

  expect_true("2" %in% names(next_state$running))
  expect_length(next_state$pending, 1)
  expect_identical(next_state$pending[[1]]$id, 1L)
})

test_that("update_queue respects slots already used by running tasks", {
  running_item <- list(
    id = 1L,
    priority = 0L,
    resources = list(slots = 1L),
    status = "running",
    task = make_scripted_task(id = 1L, status_seq = "running")
  )
  pending_item <- list(
    id = 2L,
    priority = 1L,
    submit_time = 1,
    resources = list(slots = 1L)
  )

  state <- make_tick_state(
    capacity_slots = 1L,
    running = list("1" = running_item),
    pending = list(pending_item)
  )
  next_state <- taskr:::update_queue(
    state = state,
    start_task_fn = function(item) make_scripted_task(id = item$id, status_seq = "running")
  )

  expect_true("1" %in% names(next_state$running))
  expect_false("2" %in% names(next_state$running))
  expect_length(next_state$pending, 1)
})

test_that("update_queue can launch using per-item start_task when global launcher is missing", {
  launched <- FALSE
  pending_item <- list(
    id = 1L,
    priority = 0L,
    submit_time = 1,
    resources = list(slots = 1L),
    start_task = function(item) {
      launched <<- TRUE
      make_scripted_task(id = item$id, status_seq = "running")
    }
  )

  state <- make_tick_state(capacity_slots = 1L, pending = list(pending_item))
  next_state <- taskr:::update_queue(state = state)

  expect_true(launched)
  expect_true("1" %in% names(next_state$running))
})

test_that("update_queue errors clearly when no launcher is available", {
  pending_item <- list(
    id = 1L,
    priority = 0L,
    submit_time = 1,
    resources = list(slots = 1L)
  )

  state <- make_tick_state(capacity_slots = 1L, pending = list(pending_item))

  expect_error(
    taskr:::update_queue(state = state),
    "must provide `start_task`"
  )
})

test_that("update_queue marks scheduler_should_stop only when pending and running are both empty", {
  idle_state <- make_tick_state(capacity_slots = 1L, pending = list(), running = list())
  expect_true(taskr:::update_queue(idle_state)$scheduler_should_stop)

  queued_state <- make_tick_state(
    capacity_slots = 1L,
    pending = list(list(id = 1L, priority = 0L, submit_time = 1, resources = list(slots = 2L))),
    running = list()
  )
  expect_false(taskr:::update_queue(queued_state)$scheduler_should_stop)

  running_state <- make_tick_state(
    capacity_slots = 1L,
    pending = list(),
    running = list(
      "1" = list(
        id = 1L,
        priority = 0L,
        resources = list(slots = 1L),
        status = "running",
        task = make_scripted_task(id = 1L, status_seq = "running")
      )
    )
  )
  expect_false(taskr:::update_queue(running_state)$scheduler_should_stop)
})
