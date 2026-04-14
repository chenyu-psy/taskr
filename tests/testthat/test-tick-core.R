make_tick_state <- function(capacity_slots = 2L, queue = list(), running = list(), finished = list()) {
  list(
    capacity = list(slots = as.integer(capacity_slots)),
    queue = queue,
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
  q1 <- list(id = "task_001", priority = 1L, submit_time = 2, resources = list(slots = 1L))
  q2 <- list(id = "task_002", priority = 10L, submit_time = 3, resources = list(slots = 1L))
  q3 <- list(id = "task_003", priority = 10L, submit_time = 1, resources = list(slots = 1L))

  state <- make_tick_state(capacity_slots = 2L, queue = list(q1, q2, q3))
  next_state <- taskr:::update_queue(
    state = state,
    start_task_fn = function(item) make_scripted_task(id = item$id, status_seq = "running")
  )

  expect_identical(names(next_state$running), c("task_003", "task_002"))
  expect_length(next_state$queue, 1)
  expect_identical(next_state$queue[[1]]$id, "task_001")
})

test_that("update_queue skips queued tasks with dashboard cancel markers", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  old_cancel_dir <- pkg_env$dashboard_cancel_dir
  on.exit({
    pkg_env$dashboard_cancel_dir <- old_cancel_dir
  }, add = TRUE)

  pkg_env$dashboard_cancel_dir <- tempfile("taskr-cancel-")
  dir.create(pkg_env$dashboard_cancel_dir, recursive = TRUE)

  q1 <- list(id = "task_cancel", priority = 10L, submit_time = 1, resources = list(slots = 1L))
  q2 <- list(id = "task_run", priority = 1L, submit_time = 2, resources = list(slots = 1L))
  taskr:::write_dashboard_cancel_marker("task_cancel")

  state <- make_tick_state(capacity_slots = 1L, queue = list(q1, q2))
  next_state <- taskr:::update_queue(
    state = state,
    start_task_fn = function(item) make_scripted_task(id = item$id, status_seq = "running")
  )

  expect_true("task_cancel" %in% names(next_state$finished))
  expect_equal(next_state$finished$task_cancel$status, "cancelled")
  expect_true("task_run" %in% names(next_state$running))
  expect_false(taskr:::dashboard_cancel_requested("task_cancel"))
})

test_that("update_queue cancels running tasks with dashboard cancel markers", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  old_cancel_dir <- pkg_env$dashboard_cancel_dir
  on.exit({
    pkg_env$dashboard_cancel_dir <- old_cancel_dir
  }, add = TRUE)

  pkg_env$dashboard_cancel_dir <- tempfile("taskr-cancel-")
  dir.create(pkg_env$dashboard_cancel_dir, recursive = TRUE)

  task <- make_scripted_task(id = "task_running", status_seq = "running")
  running_item <- list(
    id = "task_running",
    priority = 0L,
    resources = list(slots = 1L),
    status = "running",
    task = task
  )
  taskr:::write_dashboard_cancel_marker("task_running")

  state <- make_tick_state(capacity_slots = 1L, running = list(task_running = running_item))
  next_state <- taskr:::update_queue(state)

  expect_length(next_state$running, 0)
  expect_true("task_running" %in% names(next_state$finished))
  expect_equal(next_state$finished$task_running$status, "cancelled")
  expect_false(taskr:::dashboard_cancel_requested("task_running"))
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
    id = "task_finished",
    label = "finished_label",
    status = "completed",
    resources = list(slots = 1L),
    result_path = NULL
  )
  state <- make_tick_state(capacity_slots = 1L, finished = list(task_finished = finished_item))
  state$label_index <- list(finished_label = "task_finished")
  taskr:::write_dashboard_clean_finished_marker()

  next_state <- taskr:::update_queue(state)

  expect_length(next_state$finished, 0)
  expect_null(next_state$label_index$finished_label)
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

  queued_item <- list(
    id = "task_queued",
    label = "queued_label",
    status = "queued",
    priority = 0L,
    submit_time = 1,
    resources = list(slots = 1L),
    result_path = NULL
  )
  old_finished <- list(
    id = "task_old",
    label = "old_label",
    status = "completed",
    resources = list(slots = 1L),
    result_path = NULL
  )
  state <- make_tick_state(
    capacity_slots = 1L,
    queue = list(queued_item),
    finished = list(task_old = old_finished)
  )
  state$label_index <- list(queued_label = "task_queued", old_label = "task_old")
  taskr:::write_dashboard_cancel_marker("task_queued")
  taskr:::write_dashboard_clean_finished_marker()

  next_state <- taskr:::update_queue(
    state = state,
    start_task_fn = function(item) make_scripted_task(id = item$id, status_seq = "running")
  )

  expect_length(next_state$queue, 0)
  expect_length(next_state$running, 0)
  expect_length(next_state$finished, 0)
  expect_null(next_state$label_index$queued_label)
  expect_null(next_state$label_index$old_label)
  expect_false(taskr:::dashboard_cancel_requested("task_queued"))
  expect_false(taskr:::dashboard_clean_finished_requested())
})

test_that("update_queue recycles terminal running tasks into finished", {
  running_item <- list(
    id = "task_010",
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
    running = list(task_010 = running_item),
    queue = list()
  )

  next_state <- taskr:::update_queue(state)

  expect_length(next_state$running, 0)
  expect_true("task_010" %in% names(next_state$finished))
  expect_equal(next_state$finished$task_010$status, "completed")
  expect_equal(next_state$finished$task_010$stdout_buffer, "hello\n")
  expect_equal(next_state$finished$task_010$progress, 1)
})

test_that("update_queue recycles failed and cancelled tasks into finished with metadata", {
  failed_item <- list(
    id = "task_fail",
    priority = 0L,
    resources = list(slots = 1L),
    status = "running",
    task = make_scripted_task(
      id = "task_fail",
      status_seq = "failed",
      output = "partial\n",
      error_output = "boom\n",
      error_message = "boom"
    )
  )
  cancelled_item <- list(
    id = "task_cancelled",
    priority = 0L,
    resources = list(slots = 1L),
    status = "running",
    task = make_scripted_task(
      id = "task_cancelled",
      status_seq = "cancelled",
      output = "",
      error_output = ""
    )
  )

  state <- make_tick_state(
    capacity_slots = 2L,
    running = list(task_fail = failed_item, task_cancelled = cancelled_item),
    queue = list()
  )

  next_state <- taskr:::update_queue(state)

  expect_length(next_state$running, 0)
  expect_equal(next_state$finished$task_fail$status, "failed")
  expect_equal(next_state$finished$task_fail$error, "boom")
  expect_equal(next_state$finished$task_fail$stderr_buffer, "boom\n")
  expect_equal(next_state$finished$task_cancelled$status, "cancelled")
})

test_that("update_queue skips oversized head task and can start later smaller task", {
  big_task <- list(id = "task_big", priority = 9L, submit_time = 1, resources = list(slots = 3L))
  small_task <- list(id = "task_small", priority = 1L, submit_time = 2, resources = list(slots = 1L))

  state <- make_tick_state(capacity_slots = 2L, queue = list(big_task, small_task))
  next_state <- taskr:::update_queue(
    state = state,
    start_task_fn = function(item) make_scripted_task(id = item$id, status_seq = "running")
  )

  expect_true("task_small" %in% names(next_state$running))
  expect_length(next_state$queue, 1)
  expect_identical(next_state$queue[[1]]$id, "task_big")
})

test_that("update_queue respects slots already used by running tasks", {
  running_item <- list(
    id = "task_run",
    priority = 0L,
    resources = list(slots = 1L),
    status = "running",
    task = make_scripted_task(id = "task_run", status_seq = "running")
  )
  queued_item <- list(
    id = "task_wait",
    priority = 1L,
    submit_time = 1,
    resources = list(slots = 1L)
  )

  state <- make_tick_state(
    capacity_slots = 1L,
    running = list(task_run = running_item),
    queue = list(queued_item)
  )
  next_state <- taskr:::update_queue(
    state = state,
    start_task_fn = function(item) make_scripted_task(id = item$id, status_seq = "running")
  )

  expect_true("task_run" %in% names(next_state$running))
  expect_false("task_wait" %in% names(next_state$running))
  expect_length(next_state$queue, 1)
})

test_that("update_queue can launch using per-item start_task when global launcher is missing", {
  launched <- FALSE
  queued_item <- list(
    id = "task_item_launcher",
    priority = 0L,
    submit_time = 1,
    resources = list(slots = 1L),
    start_task = function(item) {
      launched <<- TRUE
      make_scripted_task(id = item$id, status_seq = "running")
    }
  )

  state <- make_tick_state(capacity_slots = 1L, queue = list(queued_item))
  next_state <- taskr:::update_queue(state = state)

  expect_true(launched)
  expect_true("task_item_launcher" %in% names(next_state$running))
})

test_that("update_queue errors clearly when no launcher is available", {
  queued_item <- list(
    id = "task_no_launcher",
    priority = 0L,
    submit_time = 1,
    resources = list(slots = 1L)
  )

  state <- make_tick_state(capacity_slots = 1L, queue = list(queued_item))

  expect_error(
    taskr:::update_queue(state = state),
    "must provide `start_task`"
  )
})

test_that("update_queue marks scheduler_should_stop only when queue and running are both empty", {
  idle_state <- make_tick_state(capacity_slots = 1L, queue = list(), running = list())
  expect_true(taskr:::update_queue(idle_state)$scheduler_should_stop)

  queued_state <- make_tick_state(
    capacity_slots = 1L,
    queue = list(list(id = "task_q", priority = 0L, submit_time = 1, resources = list(slots = 2L))),
    running = list()
  )
  expect_false(taskr:::update_queue(queued_state)$scheduler_should_stop)

  running_state <- make_tick_state(
    capacity_slots = 1L,
    queue = list(),
    running = list(
      task_r = list(
        id = "task_r",
        priority = 0L,
        resources = list(slots = 1L),
        status = "running",
        task = make_scripted_task(id = "task_r", status_seq = "running")
      )
    )
  )
  expect_false(taskr:::update_queue(running_state)$scheduler_should_stop)
})
