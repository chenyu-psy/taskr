wait_for_task_status <- function(list_tasks_fn, id_or_label, status, timeout = 10) {
  deadline <- Sys.time() + timeout

  while (Sys.time() < deadline) {
    tab <- list_tasks_fn()
    if (nrow(tab) > 0) {
      hit <- tab[tab$id == id_or_label | tab$label == id_or_label, , drop = FALSE]
      if (nrow(hit) == 1 && identical(hit$status[[1]], status)) {
        return(TRUE)
      }
    }
    Sys.sleep(0.05)
  }

  FALSE
}

test_that("integration: submit_task -> done -> task_result", {
  skip_if_not_installed("callr")
  list_tasks <- getFromNamespace("list_tasks", "taskr")
  task_result <- getFromNamespace("task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_task(
    expr = { Sys.sleep(0.2); 2L + 3L },
    label = "int_submit_done",
    resources = list(slots = 1L),
    output = "all"
  )

  expect_true(wait_for_task_status(list_tasks, "int_submit_done", "done", timeout = 15))
  expect_identical(task_result("int_submit_done"), 5L)
})

test_that("integration: submit_call output filtering works end-to-end", {
  skip_if_not_installed("callr")
  list_tasks <- getFromNamespace("list_tasks", "taskr")
  task_result <- getFromNamespace("task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_call(
    fun = function(x, y) list(sum = x + y, prod = x * y),
    args = list(x = 4L, y = 5L),
    label = "int_call_filter",
    resources = list(slots = 1L),
    output = "sum"
  )

  expect_true(wait_for_task_status(list_tasks, "int_call_filter", "done", timeout = 15))
  expect_identical(task_result("int_call_filter"), list(sum = 9L))
})

test_that("integration: failed task is visible and task_result errors", {
  skip_if_not_installed("callr")
  list_tasks <- getFromNamespace("list_tasks", "taskr")
  task_result <- getFromNamespace("task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_task(
    expr = { stop("integration boom") },
    label = "int_fail",
    resources = list(slots = 1L),
    output = "all"
  )

  expect_true(wait_for_task_status(list_tasks, "int_fail", "failed", timeout = 15))
  expect_error(task_result("int_fail"))
})

test_that("integration: cancel_task kills running task", {
  skip_if_not_installed("callr")
  list_tasks <- getFromNamespace("list_tasks", "taskr")
  task_result <- getFromNamespace("task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_task(
    expr = { Sys.sleep(5); 999L },
    label = "int_cancel",
    resources = list(slots = 1L),
    output = "all"
  )

  expect_true(wait_for_task_status(list_tasks, "int_cancel", "running", timeout = 15))
  taskr::cancel_task("int_cancel")
  expect_true(wait_for_task_status(list_tasks, "int_cancel", "killed", timeout = 15))
  expect_error(task_result("int_cancel"), "killed")
})
