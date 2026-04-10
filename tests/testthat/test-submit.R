find_task_in_scheduler <- function(state, id = NULL, label = NULL) {
  items <- c(state$queue %||% list(), unname(state$running %||% list()), unname(state$done %||% list()))
  if (length(items) == 0) {
    return(NULL)
  }

  hits <- Filter(
    function(x) {
      id_ok <- is.null(id) || identical(x$id %||% NA_character_, id)
      label_ok <- is.null(label) || identical(x$label %||% NA_character_, label)
      id_ok && label_ok
    },
    items
  )

  if (length(hits) == 0) return(NULL)
  hits[[1]]
}

test_that("submit_task auto-initializes queue when needed", {
  skip_if_not_installed("callr")
  taskr::shutdown_queue()
  taskr::submit_task({ 1 + 1 }, resources = list(slots = 1L))

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  expect_true(!is.null(pkg_env$scheduler))
  expect_true(pkg_env$scheduler$capacity$slots >= 1L)
})

test_that("submit_task enqueues with default output saving", {
  skip_if_not_installed("callr")
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_task(
    expr = { 1 + 1 },
    label = "expr_task",
    resources = list(slots = 2L),
    output = "all"
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  expect_equal(pkg_env$scheduler$next_id, 2L)
  item <- find_task_in_scheduler(pkg_env$scheduler, id = "task_001", label = "expr_task")
  expect_false(is.null(item))
  expect_identical(item$id, "task_001")
  expect_identical(item$label, "expr_task")
  expect_true(item$save_result)
  expect_identical(item$result_path, taskr:::task_tmpfile("task_001"))
})

test_that("submit_task supports output = none", {
  skip_if_not_installed("callr")
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_task(
    expr = { "side effect only" },
    resources = list(slots = 2L),
    output = "none"
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  item <- find_task_in_scheduler(pkg_env$scheduler, id = "task_001")
  expect_false(is.null(item))

  expect_false(item$save_result)
  expect_null(item$result_path)
})

test_that("submit_task import = auto captures globals from expression", {
  skip_if_not_installed("callr")
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  assign("taskr_auto_import_x", 10L, envir = .GlobalEnv)
  on.exit(rm("taskr_auto_import_x", envir = .GlobalEnv), add = TRUE)

  taskr::submit_task(
    expr = { taskr_auto_import_x + 1L },
    resources = list(slots = 2L),
    import = "auto"
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  item <- find_task_in_scheduler(pkg_env$scheduler, id = "task_001")
  expect_false(is.null(item))

  expect_true("taskr_auto_import_x" %in% item$context$env)
  expect_true(is.character(item$context$packages))
  expect_identical(item$context$workdir, getwd())
})

test_that("submit_task import = none disables context injection", {
  skip_if_not_installed("callr")
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_task(
    expr = { 1 + 1 },
    resources = list(slots = 2L),
    import = "none"
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  item <- find_task_in_scheduler(pkg_env$scheduler, id = "task_001")
  expect_false(is.null(item))

  expect_identical(item$context$env, character())
  expect_identical(item$context$packages, character())
  expect_null(item$context$workdir)
  expect_null(item$context$vars)
})

test_that("submit_task import list supports explicit context fields", {
  skip_if_not_installed("callr")
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)
  assign("df", data.frame(x = 1), envir = .GlobalEnv)
  on.exit(rm("df", envir = .GlobalEnv), add = TRUE)

  taskr::submit_task(
    expr = { 1 + 1 },
    resources = list(slots = 2L),
    import = list(
      env = "df",
      packages = "stats",
      workdir = tempdir(),
      vars = list(TASKR_FLAG = "yes")
    )
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  item <- find_task_in_scheduler(pkg_env$scheduler, id = "task_001")
  expect_false(is.null(item))

  expect_identical(item$context$env, "df")
  expect_identical(item$context$packages, "stats")
  expect_identical(item$context$workdir, tempdir())
  expect_identical(item$context$vars$TASKR_FLAG, "yes")
})

test_that("submit_task validates output values", {
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  expect_error(taskr::submit_task(expr = { 1 }, output = "some"), "one of: \"all\" or \"none\"")
  expect_error(taskr::submit_task(expr = { 1 }, output = c("all", "none")), "one of: \"all\" or \"none\"")
})

test_that("submit_task enforces unique labels", {
  skip_if_not_installed("callr")
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_task(
    expr = { 1 + 1 },
    label = "dup_label",
    resources = list(slots = 2L)
  )

  expect_error(
    taskr::submit_task(
      expr = { 2 + 2 },
      label = "dup_label",
      resources = list(slots = 2L)
    ),
    "already exists"
  )
})

test_that("submit_task rejects resource requests above queue capacity", {
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  expect_error(
    taskr::submit_task(
      expr = { 1 + 1 },
      resources = list(slots = 2L)
    ),
    "queue capacity is 1"
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  expect_length(pkg_env$scheduler$queue, 0)
})

test_that("build_submit_call_expr filters selected fields", {
  expr <- taskr:::build_submit_call_expr(
    fun = function(a, b) list(sum = a + b, prod = a * b),
    args = list(a = 2, b = 4),
    field_filter = "sum"
  )

  expect_equal(eval(expr), list(sum = 6))
})

test_that("build_submit_call_expr fails when output filtering is incompatible", {
  expr_non_list <- taskr:::build_submit_call_expr(
    fun = function() 123,
    args = list(),
    field_filter = "x"
  )
  expr_missing_field <- taskr:::build_submit_call_expr(
    fun = function() list(a = 1),
    args = list(),
    field_filter = c("a", "b")
  )

  expect_error(eval(expr_non_list), "requires `fun` to return a named list")
  expect_error(eval(expr_missing_field), "Requested output field")
})

test_that("submit_call accepts field filtering output", {
  skip_if_not_installed("callr")
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_call(
    fun = function() list(keep = 1, drop = 2),
    args = list(),
    output = "keep",
    resources = list(slots = 2L),
    label = "call_filter"
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  item <- find_task_in_scheduler(pkg_env$scheduler, id = "task_001", label = "call_filter")
  expect_false(is.null(item))

  expect_identical(item$id, "task_001")
  expect_identical(item$output, "keep")
  expect_true(item$save_result)
  expect_identical(item$label, "call_filter")
})

test_that("submit_call supports output = none", {
  skip_if_not_installed("callr")
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  taskr::submit_call(
    fun = function() 42,
    args = list(),
    output = "none",
    resources = list(slots = 2L)
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  item <- find_task_in_scheduler(pkg_env$scheduler, id = "task_001")
  expect_false(is.null(item))

  expect_false(item$save_result)
  expect_null(item$result_path)
})

test_that("submit_call import = auto can capture globals from function body", {
  skip_if_not_installed("callr")
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  assign("taskr_auto_fun_global", 3L, envir = .GlobalEnv)
  on.exit(rm("taskr_auto_fun_global", envir = .GlobalEnv), add = TRUE)

  taskr::submit_call(
    fun = function(x) x + taskr_auto_fun_global,
    args = list(x = 1L),
    resources = list(slots = 2L),
    import = "auto"
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  item <- find_task_in_scheduler(pkg_env$scheduler, id = "task_001")
  expect_false(is.null(item))

  expect_true("taskr_auto_fun_global" %in% item$context$env)
})

test_that("submit_call rejects resource requests above queue capacity", {
  taskr::shutdown_queue()
  taskr::init_queue(max_concurrent = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  expect_error(
    taskr::submit_call(
      fun = function(x) x,
      args = list(x = 1L),
      resources = list(slots = 3L)
    ),
    "queue capacity is 2"
  )
})
