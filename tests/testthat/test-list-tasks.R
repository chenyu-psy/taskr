make_scheduler_item <- function(
    id,
    label,
    status,
    priority = 0L,
    slots = 1L,
    output = "all",
    progress = NA_real_,
    message = NA_character_,
    error = NULL) {
  now <- Sys.time()
  list(
    id = id,
    label = label,
    status = status,
    priority = priority,
    resources = list(slots = slots),
    output = output,
    progress = progress,
    message = message,
    submit_time = now - 10,
    start_time = if (status %in% c("running", "completed", "failed", "cancelled")) now - 5 else as.POSIXct(NA),
    end_time = if (status %in% c("completed", "failed", "cancelled")) now - 1 else as.POSIXct(NA),
    result_path = tempfile(fileext = ".rds"),
    error = error
  )
}

test_that("get_task_overview returns an empty data frame when queue is not initialized", {
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  taskr::shutdown_queue()

  out <- get_task_overview()

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
  expect_true(all(
    c("id", "label", "status", "progress", "message", "elapsed", "error") %in% names(out)
  ))
})

test_that("get_task_overview returns records from pending, running, and finished", {
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  q <- make_scheduler_item(1L, "pending_a", "pending")
  r <- make_scheduler_item(2L, "running_a", "running", progress = 0.5, message = "half")
  d <- make_scheduler_item(3L, "completed_a", "completed")

  pkg_env$scheduler <- new_scheduler_state(max_slots = 2)
  pkg_env$scheduler$pending <- list(q)
  pkg_env$scheduler$running <- list("2" = r)
  pkg_env$scheduler$finished <- list("3" = d)

  out <- get_task_overview()

  expect_equal(nrow(out), 3)
  expect_setequal(out$id, c(1L, 2L, 3L))
  expect_equal(out$message[out$id == 2L], "half")
  expect_true(is.character(out$elapsed))
})

test_that("get_task_overview applies id/label/status filters with AND logic", {
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  a <- make_scheduler_item(11L, "fit_a", "pending")
  b <- make_scheduler_item(12L, "fit_b", "running")
  c <- make_scheduler_item(13L, "fit_b", "completed")

  pkg_env$scheduler <- new_scheduler_state(max_slots = 2)
  pkg_env$scheduler$pending <- list(a)
  pkg_env$scheduler$running <- list("12" = b)
  pkg_env$scheduler$finished <- list("13" = c)

  out1 <- get_task_overview(status = "running")
  expect_equal(nrow(out1), 1)
  expect_identical(out1$id, 12L)

  out2 <- get_task_overview(label = "fit_b")
  expect_equal(nrow(out2), 2)

  out3 <- get_task_overview(label = "fit_b", status = "completed")
  expect_equal(nrow(out3), 1)
  expect_identical(out3$id, 13L)
})

test_that("get_task_overview sorts rows by status group then id", {
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  q <- make_scheduler_item(9L, "q", "pending")
  r <- make_scheduler_item(4L, "r", "running")
  d2 <- make_scheduler_item(10L, "d2", "completed")
  d1 <- make_scheduler_item(1L, "d1", "completed")

  pkg_env$scheduler <- new_scheduler_state(max_slots = 2)
  pkg_env$scheduler$pending <- list(q)
  pkg_env$scheduler$running <- list("4" = r)
  pkg_env$scheduler$finished <- list("10" = d2, "1" = d1)

  out <- get_task_overview()
  expect_identical(out$status, c("completed", "completed", "running", "pending"))
  expect_identical(out$id, c(1L, 10L, 4L, 9L))
})

test_that("get_task_overview validates filter types", {
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  expect_error(get_task_overview(id = 1.5), "positive integer")
  expect_error(get_task_overview(label = NA_character_), "without missing values")
  expect_error(get_task_overview(status = NA_character_), "without missing values")
})

test_that("get_task_overview restarts scheduler when pending work exists", {
  skip_if_not_installed("later")
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")
  stop_scheduler <- getFromNamespace("stop_scheduler", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$pending <- list(
    list(
      id = 1L,
      label = "restart_demo",
      status = "pending",
      resources = list(slots = 1L),
      priority = 0L,
      submit_time = Sys.time(),
      output = "none",
      start_task = function(item) {
        list(
          id = item$id,
          error = NULL,
          status = function() "running",
          progress = function() NULL,
          read_output = function() "",
          read_error = function() "",
          is_alive = function() TRUE,
          kill = function() invisible(NULL),
          elapsed = function() 0
        )
      }
    )
  )
  pkg_env$scheduler$scheduler_handle <- NULL

  get_task_overview()
  expect_true(is.function(pkg_env$scheduler$scheduler_handle))

  stop_scheduler()
})
