test_that("queue_overview prints not-initialized message", {
  queue_overview <- getFromNamespace("queue_overview", "taskr")

  taskr::shutdown_queue()
  out <- c(
    capture.output(queue_overview(), type = "output"),
    capture.output(queue_overview(), type = "message")
  )

  expect_true(any(grepl("queue", out, ignore.case = TRUE)))
  expect_true(any(grepl("not initialized", out, ignore.case = TRUE)))
})

test_that("queue_overview prints capacity and status counts", {
  queue_overview <- getFromNamespace("queue_overview", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 3)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$scheduler <- new_scheduler_state(max_concurrent = 3)
  pkg_env$scheduler$queue <- list(
    list(id = "task_1", label = "q1", status = "queued", resources = list(slots = 1L))
  )
  pkg_env$scheduler$running <- list(
    task_2 = list(id = "task_2", label = "r1", status = "running", resources = list(slots = 2L))
  )
  pkg_env$scheduler$done <- list(
    task_3 = list(id = "task_3", label = "d1", status = "done", resources = list(slots = 1L)),
    task_4 = list(id = "task_4", label = "f1", status = "failed", resources = list(slots = 1L)),
    task_5 = list(id = "task_5", label = "k1", status = "killed", resources = list(slots = 1L))
  )

  out <- c(
    capture.output(queue_overview(), type = "output"),
    capture.output(queue_overview(), type = "message")
  )
  text <- paste(out, collapse = "\n")

  expect_true(grepl("Capacity:\\s*2/3", text))
  expect_true(grepl("Queued:\\s*1", text))
  expect_true(grepl("Running:\\s*1", text))
  expect_true(grepl("Terminal:\\s*3", text))
})
