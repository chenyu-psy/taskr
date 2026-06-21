wait_for_task_status <- function(list_tasks_fn, label, status, timeout = 10) {
  deadline <- Sys.time() + timeout

  while (Sys.time() < deadline) {
    tab <- list_tasks_fn(label = label)
    if (nrow(tab) > 0) {
      if (nrow(tab) == 1 && identical(tab$status[[1]], status)) {
        return(TRUE)
      }
    }
    Sys.sleep(0.05)
  }

  FALSE
}

task_id_for_label <- function(list_tasks_fn, label) {
  tab <- list_tasks_fn(label = label)
  if (nrow(tab) != 1) {
    stop("Expected one task for label `", label, "`.")
  }
  tab$id[[1]]
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
  expect_identical(get_task_result(task_id_for_label(get_task_overview, "int_submit_done")), 5L)
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
  task_id <- task_id_for_label(get_task_overview, "int_default_none")
  expect_warning(
    expect_null(get_task_result(task_id)),
    "output = \"none\""
  )
})

test_that("integration: output none completes when user code saves an external result", {
  skip_if_not_installed("callr")
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  get_task_result <- getFromNamespace("get_task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  external_path <- tempfile(fileext = ".rds")
  on.exit({
    taskr::shutdown_queue()
    unlink(external_path)
  }, add = TRUE)

  taskr::submit_code(
    expr = {
      fit_summary <- list(model = "stan_like", draws = 100L)
      saveRDS(fit_summary, Sys.getenv("TASKR_EXTERNAL_RESULT"))
      fit_summary
    },
    label = "int_external_result_none",
    resources = list(slots = 1L),
    import = list(vars = list(TASKR_EXTERNAL_RESULT = external_path)),
    output = "none"
  )

  expect_true(wait_for_task_status(get_task_overview, "int_external_result_none", "completed", timeout = 15))
  task_id <- task_id_for_label(get_task_overview, "int_external_result_none")
  expect_true(file.exists(external_path))
  expect_identical(readRDS(external_path), list(model = "stan_like", draws = 100L))
  expect_warning(
    expect_null(get_task_result(task_id)),
    "output = \"none\""
  )
})

test_that("integration: submit_task output none completes without a result file", {
  skip_if_not_installed("callr")
  get_task_overview <- getFromNamespace("get_task_overview", "taskr")
  get_task_result <- getFromNamespace("get_task_result", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_task(
    fun = function(x) x + 1L,
    args = list(x = 41L),
    label = "int_call_none",
    resources = list(slots = 1L),
    output = "none"
  )

  expect_true(wait_for_task_status(get_task_overview, "int_call_none", "completed", timeout = 15))
  task_id <- task_id_for_label(get_task_overview, "int_call_none")
  expect_warning(
    expect_null(get_task_result(task_id)),
    "output = \"none\""
  )
})

test_that("integration: brms cmdstanr output none completes after model file save", {
  skip_if_not_installed("callr")
  skip_if_not_installed("brms")
  skip_if_not_installed("cmdstanr")
  cmdstan_version <- cmdstanr::cmdstan_version(error_on_NA = FALSE)
  if (length(cmdstan_version) == 0 || is.na(cmdstan_version)) {
    skip("CmdStan is not installed.")
  }

  get_task_overview <- getFromNamespace("get_task_overview", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  model_file <- tempfile(pattern = "taskr-brms-cmdstanr-")
  on.exit({
    taskr::shutdown_queue()
    unlink(paste0(model_file, ".rds"), force = TRUE)
  }, add = TRUE)

  tiny_data <- data.frame(y = c(-0.2, 0.1, 0.3, -0.1, 0.2, 0.0))
  taskr::submit_task(
    fun = brms::brm,
    args = list(
      formula = y ~ 1,
      data = tiny_data,
      backend = "cmdstanr",
      chains = 1L,
      cores = 1L,
      iter = 20L,
      warmup = 10L,
      seed = 1234L,
      refresh = 0L,
      file = model_file,
      file_refit = "always"
    ),
    label = "int_brms_cmdstanr_none",
    resources = list(slots = 1L),
    import = list(packages = "brms", workdir = getwd()),
    output = "none"
  )

  expect_true(wait_for_task_status(
    get_task_overview,
    "int_brms_cmdstanr_none",
    "completed",
    timeout = 120
  ))
  expect_true(file.exists(paste0(model_file, ".rds")))
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
  expect_identical(get_task_result(task_id_for_label(get_task_overview, "int_call_filter")), list(sum = 9L))
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
  expect_error(get_task_result(task_id_for_label(get_task_overview, "int_fail")))
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
  task_id <- task_id_for_label(get_task_overview, "int_cancel")
  taskr::cancel_task(task_id)
  expect_true(wait_for_task_status(get_task_overview, "int_cancel", "cancelled", timeout = 15))
  expect_error(get_task_result(task_id), "cancelled")
})
