collect_scheduler_items_for_test <- function(state) {
  c(state$pending %||% list(), unname(state$running %||% list()), unname(state$finished %||% list()))
}

test_that("map_tasks enqueues one task per data.frame row", {
  skip_if_not_installed("callr")
  map_tasks <- getFromNamespace("map_tasks", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  grid <- data.frame(a = c(1L, 2L), stringsAsFactors = FALSE)
  map_tasks(
    fun = function(x) x + 1L,
    grid = grid,
    fixed_args = list(x = 10L),
    resources = list(slots = 2L)
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  items <- collect_scheduler_items_for_test(pkg_env$scheduler)
  expect_length(items, 2)
  expect_equal(pkg_env$scheduler$next_id, 3L)
  expect_true(all(vapply(items, function(x) identical(x$output, "none"), logical(1))))
  expect_true(all(vapply(items, function(x) identical(x$save_result, FALSE), logical(1))))
})

test_that("map_tasks supports fun_ dynamic generators", {
  skip_if_not_installed("callr")
  map_tasks <- getFromNamespace("map_tasks", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  grid <- data.frame(k = c(2L, 3L), stringsAsFactors = FALSE)
  map_tasks(
    fun = function(x) x * 2L,
    grid = grid,
    fun_x = function(k) k + 1L,
    resources = list(slots = 2L)
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  items <- collect_scheduler_items_for_test(pkg_env$scheduler)
  values <- vapply(items, function(item) eval(item$expr), integer(1))
  values <- sort(values)
  expect_identical(values, c(6L, 8L))
})

test_that("map_tasks supports label templates with grid variables", {
  skip_if_not_installed("callr")
  map_tasks <- getFromNamespace("map_tasks", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  grid <- data.frame(model = c("m1", "m2"), stringsAsFactors = FALSE)
  map_tasks(
    fun = function(x) x,
    grid = grid,
    fun_x = function(model) model,
    label_fmt = "fit_{model}",
    resources = list(slots = 2L)
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  items <- collect_scheduler_items_for_test(pkg_env$scheduler)
  labels <- sort(vapply(items, `[[`, character(1), "label"))
  expect_identical(labels, c("fit_m1", "fit_m2"))
})

test_that("map_tasks supports list-based grids", {
  skip_if_not_installed("callr")
  map_tasks <- getFromNamespace("map_tasks", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 2)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  grid <- list(
    list(a = 1L, b = 2L),
    list(a = 3L, b = 4L)
  )
  map_tasks(
    fun = function(x) x,
    grid = grid,
    fun_x = function(a, b) a + b,
    resources = list(slots = 2L)
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  items <- collect_scheduler_items_for_test(pkg_env$scheduler)
  values <- vapply(items, function(item) eval(item$expr), integer(1))
  values <- sort(values)
  expect_identical(values, c(3L, 7L))
})

test_that("map_tasks validates dynamic generator inputs", {
  map_tasks <- getFromNamespace("map_tasks", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  grid <- data.frame(a = 1L, stringsAsFactors = FALSE)

  expect_error(
    map_tasks(fun = function(x) x, grid = grid, fun_x = 123),
    "must be functions"
  )

  expect_error(
    map_tasks(fun = function(x) x, grid = grid, fun_x = function(missing_col) missing_col),
    "missing grid variable"
  )
})

test_that("map_tasks validates label_fmt placeholders", {
  map_tasks <- getFromNamespace("map_tasks", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  grid <- data.frame(a = 1L, stringsAsFactors = FALSE)

  expect_error(
    map_tasks(
      fun = function(x) x,
      grid = grid,
      fun_x = function(a) a,
      label_fmt = "fit_{missing}"
    ),
    "missing grid variable"
  )
})

test_that("map_tasks fails early when resources exceed queue capacity", {
  map_tasks <- getFromNamespace("map_tasks", "taskr")
  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  grid <- data.frame(a = c(1L, 2L), stringsAsFactors = FALSE)
  expect_error(
    map_tasks(
      fun = function(x) x,
      grid = grid,
      fun_x = function(a) a,
      resources = list(slots = 2L)
    ),
    "queue capacity is 1"
  )

  pkg_env <- getFromNamespace("pkg_env", "taskr")
  expect_length(pkg_env$scheduler$pending, 0)
})
