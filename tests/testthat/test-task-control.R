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

test_that("control_cancel_task cancels queued task through main console state", {
  control_cancel_task <- getFromNamespace("control_cancel_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pending_item <- make_control_item("task_002", "queued_control", "queued")
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$queue <- list(pending_item)

  out <- control_cancel_task("task_002")

  expect_true(out$ok)
  expect_identical(out$status, "cancelled")
  expect_length(pkg_env$scheduler$queue, 0)
  expect_identical(pkg_env$scheduler$finished$task_002$status, "cancelled")
})

test_that("control_clear_all_tasks removes queued tasks and kills active tasks", {
  control_clear_all_tasks <- getFromNamespace("control_clear_all_tasks", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  task_obj <- make_killable_running_task()
  running_item <- make_control_item("task_003", "running_control", "running", task = task_obj)
  queued_item <- make_control_item("task_004", "queued_control", "queued")
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$running <- list(task_003 = running_item)
  pkg_env$scheduler$queue <- list(queued_item)

  out <- control_clear_all_tasks()

  expect_true(out$ok)
  expect_true(task_obj$state$cancelled)
  expect_identical(out$cancelled, "task_003")
  expect_identical(out$removed_queued, "task_004")
  expect_length(pkg_env$scheduler$running, 0)
  expect_length(pkg_env$scheduler$queue, 0)
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

test_that("remove_task removes one failed task and frees its label", {
  remove_task <- getFromNamespace("remove_task", "taskr")
  validate_unique_label <- getFromNamespace("validate_unique_label", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  failed_path <- tempfile(fileext = ".rds")
  other_path <- tempfile(fileext = ".rds")
  saveRDS("failed", failed_path)
  saveRDS("other", other_path)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    task_040 = make_control_item("task_040", "failed_a", "failed", path = failed_path),
    task_041 = make_control_item("task_041", "done_b", "completed", path = other_path)
  )
  pkg_env$scheduler$label_index <- list(
    failed_a = "task_040",
    done_b = "task_041"
  )

  remove_task("failed_a")

  expect_false("task_040" %in% names(pkg_env$scheduler$finished))
  expect_true("task_041" %in% names(pkg_env$scheduler$finished))
  expect_false(file.exists(failed_path))
  expect_true(file.exists(other_path))
  expect_null(pkg_env$scheduler$label_index$failed_a)
  expect_identical(pkg_env$scheduler$label_index$done_b, "task_041")
  expect_silent(validate_unique_label(pkg_env$scheduler, "failed_a"))
})

test_that("remove_task removes completed and cancelled records without touching others", {
  remove_task <- getFromNamespace("remove_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    task_050 = make_control_item("task_050", "done_a", "completed"),
    task_051 = make_control_item("task_051", "cancel_b", "cancelled"),
    task_052 = make_control_item("task_052", "failed_c", "failed")
  )
  pkg_env$scheduler$label_index <- list(
    done_a = "task_050",
    cancel_b = "task_051",
    failed_c = "task_052"
  )

  remove_task("task_050")
  remove_task("cancel_b")

  expect_false("task_050" %in% names(pkg_env$scheduler$finished))
  expect_false("task_051" %in% names(pkg_env$scheduler$finished))
  expect_true("task_052" %in% names(pkg_env$scheduler$finished))
  expect_null(pkg_env$scheduler$label_index$done_a)
  expect_null(pkg_env$scheduler$label_index$cancel_b)
  expect_identical(pkg_env$scheduler$label_index$failed_c, "task_052")
})

test_that("remove_task cancels queued task and removes its terminal record", {
  remove_task <- getFromNamespace("remove_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  queued_item <- make_control_item("task_060", "queued_remove", "queued")
  other_item <- make_control_item("task_061", "queued_keep", "queued")
  other_item$start_task <- function(item) make_killable_running_task()
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$queue <- list(queued_item, other_item)
  pkg_env$scheduler$label_index <- list(
    queued_remove = "task_060",
    queued_keep = "task_061"
  )

  remove_task("queued_remove")

  remaining_ids <- names(pkg_env$scheduler$running %||% list())
  remaining_ids <- c(remaining_ids, vapply(pkg_env$scheduler$queue, function(item) item$id, character(1)))
  remaining_ids <- c(remaining_ids, names(pkg_env$scheduler$finished %||% list()))
  expect_false("task_060" %in% remaining_ids)
  expect_true("task_061" %in% remaining_ids)
  expect_false("task_060" %in% names(pkg_env$scheduler$finished))
  expect_null(pkg_env$scheduler$label_index$queued_remove)
  expect_identical(pkg_env$scheduler$label_index$queued_keep, "task_061")
})

test_that("remove_task cancels running task and removes its terminal record", {
  remove_task <- getFromNamespace("remove_task", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  task_obj <- make_killable_running_task()
  running_item <- make_control_item("task_070", "running_remove", "running", task = task_obj)
  done_item <- make_control_item("task_071", "done_keep", "completed")
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$running <- list(task_070 = running_item)
  pkg_env$scheduler$finished <- list(task_071 = done_item)
  pkg_env$scheduler$label_index <- list(
    running_remove = "task_070",
    done_keep = "task_071"
  )

  remove_task("running_remove")

  expect_true(task_obj$state$cancelled)
  expect_false("task_070" %in% names(pkg_env$scheduler$running))
  expect_false("task_070" %in% names(pkg_env$scheduler$finished))
  expect_true("task_071" %in% names(pkg_env$scheduler$finished))
  expect_null(pkg_env$scheduler$label_index$running_remove)
  expect_identical(pkg_env$scheduler$label_index$done_keep, "task_071")
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
