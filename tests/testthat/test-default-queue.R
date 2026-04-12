test_that("default_queue_slots returns a positive integer", {
  default_queue_slots <- getFromNamespace("default_queue_slots", "taskr")
  value <- default_queue_slots()
  expect_true(is.integer(value))
  expect_equal(length(value), 1L)
  expect_true(value >= 1L)
})

test_that("map_tasks auto-initializes queue when needed", {
  skip_if_not_installed("callr")
  map_tasks <- getFromNamespace("map_tasks", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")

  taskr::shutdown_queue()
  on.exit(taskr::shutdown_queue(), add = TRUE)

  map_tasks(
    fun = function(x) x,
    grid = data.frame(a = 1L, stringsAsFactors = FALSE),
    fun_x = function(a) a,
    resources = list(slots = 1L)
  )

  expect_true(!is.null(pkg_env$scheduler))
  expect_true(pkg_env$scheduler$capacity$slots >= 1L)
})
