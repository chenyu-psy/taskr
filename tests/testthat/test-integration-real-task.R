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

test_that("integration: submit_code -> completed -> get_task_result", {
  skip_if_not_installed("callr")
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  get_task_result <- getFromNamespace("get_task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_code(
    expr = { Sys.sleep(0.2); 2L + 3L },
    label = "int_submit_done",
    resources = list(slots = 1L),
    output = "all"
  )

  expect_true(wait_for_task_status(get_task_overview, "int_submit_done", "completed", timeout = 15))
  expect_identical(get_task_result("int_submit_done"), 5L)
})

test_that("integration: default output completes without a result file", {
  skip_if_not_installed("callr")
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  get_task_result <- getFromNamespace("get_task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_code(
    expr = { Sys.sleep(0.2); "large result not saved" },
    label = "int_default_none",
    resources = list(slots = 1L)
  )

  expect_true(wait_for_task_status(get_task_overview, "int_default_none", "completed", timeout = 15))
  expect_warning(
    expect_null(get_task_result("int_default_none")),
    "output = \"none\""
  )
})

test_that("integration: submit_task output filtering works end-to-end", {
  skip_if_not_installed("callr")
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  get_task_result <- getFromNamespace("get_task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_task(
    fun = function(x, y) list(sum = x + y, prod = x * y),
    args = list(x = 4L, y = 5L),
    label = "int_call_filter",
    resources = list(slots = 1L),
    output = "sum"
  )

  expect_true(wait_for_task_status(get_task_overview, "int_call_filter", "completed", timeout = 15))
  expect_identical(get_task_result("int_call_filter"), list(sum = 9L))
})

test_that("integration: failed task is visible and get_task_result errors", {
  skip_if_not_installed("callr")
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  get_task_result <- getFromNamespace("get_task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_code(
    expr = { stop("integration boom") },
    label = "int_fail",
    resources = list(slots = 1L),
    output = "all"
  )

  expect_true(wait_for_task_status(get_task_overview, "int_fail", "failed", timeout = 15))
  expect_error(get_task_result("int_fail"))
})

test_that("integration: cancel_task kills running task", {
  skip_if_not_installed("callr")
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  get_task_result <- getFromNamespace("get_task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_code(
    expr = { Sys.sleep(5); 999L },
    label = "int_cancel",
    resources = list(slots = 1L),
    output = "all"
  )

  expect_true(wait_for_task_status(get_task_overview, "int_cancel", "running", timeout = 15))
  taskr::cancel_task("int_cancel")
  expect_true(wait_for_task_status(get_task_overview, "int_cancel", "cancelled", timeout = 15))
  expect_error(get_task_result("int_cancel"), "cancelled")
})
