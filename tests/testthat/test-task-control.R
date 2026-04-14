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
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  path <- tempfile(fileext = ".rds")
  saveRDS(1L, path)

  q <- make_control_item("task_001", "queued_a", "queued", path = path)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$queue <- list(q)

  cancel_task("task_001")

  expect_length(pkg_env$scheduler$queue, 0)
  expect_true("task_001" %in% names(pkg_env$scheduler$finished))
  expect_identical(pkg_env$scheduler$finished$task_001$status, "cancelled")
  expect_false(file.exists(path))
})

test_that("cancel_task kills a running task", {
  cancel_task <- getFromNamespace("cancel_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  task_obj <- make_killable_running_task()
  r <- make_control_item("task_010", "run_a", "running", task = task_obj)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$running <- list(task_010 = r)

  cancel_task("run_a")

  expect_true(task_obj$state$cancelled)
  expect_length(pkg_env$scheduler$running, 0)
  expect_identical(pkg_env$scheduler$finished$task_010$status, "cancelled")
})

test_that("cancel_task kills active task when scheduler state is missing", {
  cancel_task <- getFromNamespace("cancel_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  register_active_task <- getFromNamespace("register_active_task", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  task_obj <- taskr:::Task$new(
    id = "task_active_001",
    process = make_killable_running_task(),
    status = "running"
  )
  register_active_task(task_obj)
  pkg_env$scheduler <- NULL

  cancel_task("task_active_001")

  expect_identical(task_obj$status(), "cancelled")
  expect_false(exists("task_active_001", envir = pkg_env$active_tasks, inherits = FALSE))
})

test_that("cancel_task kills active task when finished record is stale", {
  cancel_task <- getFromNamespace("cancel_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")
  register_active_task <- getFromNamespace("register_active_task", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  task_obj <- taskr:::Task$new(
    id = "task_active_002",
    process = make_killable_running_task(),
    status = "running"
  )
  register_active_task(task_obj)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    task_active_002 = make_control_item("task_active_002", "stale_done", "completed")
  )

  expect_warning(cancel_task("task_active_002"), NA)

  expect_identical(task_obj$status(), "cancelled")
  expect_identical(pkg_env$scheduler$finished$task_active_002$status, "cancelled")
  expect_false(exists("task_active_002", envir = pkg_env$active_tasks, inherits = FALSE))
})

test_that("dashboard cancel command uses session inbox and writes ack", {
  dashboard_enqueue_command <- getFromNamespace("dashboard_enqueue_command", "taskr")
  process_dashboard_commands <- getFromNamespace("process_dashboard_commands", "taskr")
  dashboard_session_id <- getFromNamespace("dashboard_session_id", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)

  old_ack_path <- pkg_env$dashboard_ack_path
  command_dir <- tempfile("commands_")
  ack_dir <- tempfile("acks_")
  dir.create(command_dir)
  dir.create(ack_dir)
  pkg_env$dashboard_ack_path <- ack_dir
  on.exit({
    pkg_env$dashboard_ack_path <- old_ack_path
    taskr::shutdown_queue()
  }, add = TRUE)

  pending_item <- make_control_item("task_002", "queued_cmd", "queued")
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$queue <- list(pending_item)

  request_id <- dashboard_enqueue_command(
    "cancel_task",
    task_id = "task_002",
    path = command_dir,
    session_id = dashboard_session_id()
  )
  processed <- process_dashboard_commands(path = command_dir)

  expect_true(processed)
  expect_length(pkg_env$scheduler$queue, 0)
  expect_identical(pkg_env$scheduler$finished$task_002$status, "cancelled")

  ack_path <- file.path(ack_dir, paste0(request_id, ".json"))
  expect_true(file.exists(ack_path))
  ack <- jsonlite::fromJSON(ack_path, simplifyVector = TRUE)
  expect_identical(ack$status, "ok")
  expect_identical(ack$result, "cancelled")
})

test_that("dashboard command path stays stable across queue sessions", {
  dashboard_command_path <- getFromNamespace("dashboard_command_path", "taskr")
  dashboard_session_id <- getFromNamespace("dashboard_session_id", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  first_session <- dashboard_session_id()
  first_command_path <- dashboard_command_path()

  taskr::init_queue(max_slots = 1)

  expect_false(identical(dashboard_session_id(), first_session))
  expect_identical(dashboard_command_path(), first_command_path)
})

test_that("dashboard command from stale session is ignored with ack", {
  dashboard_enqueue_command <- getFromNamespace("dashboard_enqueue_command", "taskr")
  process_dashboard_commands <- getFromNamespace("process_dashboard_commands", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)

  old_ack_path <- pkg_env$dashboard_ack_path
  command_dir <- tempfile("commands_")
  ack_dir <- tempfile("acks_")
  dir.create(command_dir)
  dir.create(ack_dir)
  pkg_env$dashboard_ack_path <- ack_dir
  on.exit({
    pkg_env$dashboard_ack_path <- old_ack_path
    taskr::shutdown_queue()
  }, add = TRUE)

  task_obj <- make_killable_running_task()
  running_item <- make_control_item("task_003", "running_cmd", "running", task = task_obj)
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$running <- list(task_003 = running_item)

  request_id <- dashboard_enqueue_command(
    "cancel_task",
    task_id = "task_003",
    path = command_dir,
    session_id = "stale_session"
  )
  processed <- process_dashboard_commands(path = command_dir)

  expect_false(processed)
  expect_false(task_obj$state$cancelled)
  expect_true("task_003" %in% names(pkg_env$scheduler$running))

  ack_path <- file.path(ack_dir, paste0(request_id, ".json"))
  expect_true(file.exists(ack_path))
  ack <- jsonlite::fromJSON(ack_path, simplifyVector = TRUE)
  expect_identical(ack$status, "ignored")
  expect_identical(ack$result, "session_mismatch")
})

test_that("cancel_task warns when task is already terminal", {
  cancel_task <- getFromNamespace("cancel_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  d <- make_control_item("task_020", "completed_a", "completed")
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(task_020 = d)

  expect_warning(cancel_task("task_020"), "already terminal")
})

test_that("cancel_task validates lookup behavior", {
  cancel_task <- getFromNamespace("cancel_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    task_030 = make_control_item("task_030", "dup", "completed"),
    task_031 = make_control_item("task_031", "dup", "completed")
  )

  expect_error(cancel_task("missing"), "Task not found")
  expect_error(cancel_task("dup"), "More than one task matches")
})

test_that("clean_tasks removes completed records and deletes result files", {
  clean_tasks <- getFromNamespace("clean_tasks", "taskr")
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
    task_100 = make_control_item("task_100", "a", "completed", path = p1),
    task_101 = make_control_item("task_101", "b", "failed", path = p2),
    task_102 = make_control_item("task_102", "c", "cancelled", path = NULL)
  )
  pkg_env$scheduler$label_index <- list(
    a = "task_100",
    b = "task_101",
    c = "task_102"
  )

  clean_tasks()

  expect_length(pkg_env$scheduler$finished, 0)
  expect_false(file.exists(p1))
  expect_false(file.exists(p2))
  expect_null(pkg_env$scheduler$label_index[["a"]])
  expect_null(pkg_env$scheduler$label_index[["b"]])
  expect_null(pkg_env$scheduler$label_index[["c"]])
})
