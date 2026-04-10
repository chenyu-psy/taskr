test_that("build_task_expr returns the user result invisibly", {
  wrapped_expr <- taskr:::build_task_expr(quote(1 + 1))
  result <- eval(wrapped_expr)

  expect_null(result)
  expect_equal(get(".__taskr_result", envir = environment()), 2)
  rm(".__taskr_result", envir = environment())
})

test_that("build_task_expr saves the result when a path is supplied", {
  result_path <- tempfile(fileext = ".rds")
  wrapped_expr <- taskr:::build_task_expr(quote(list(value = 42)), result_path = result_path)

  eval(wrapped_expr)

  expect_true(file.exists(result_path))
  expect_equal(readRDS(result_path)$value, 42)
  unlink(result_path)
  rm(".__taskr_result", envir = environment())
})

test_that("build_task_expr applies imports, packages, workdir, and env", {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  on.exit(Sys.unsetenv("TASKR_TEST_ENV"), add = TRUE)
  tmp_dir <- tempfile(pattern = "taskr-wd-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  wrapped_expr <- taskr:::build_task_expr(
    expr = quote(list(
      imported_value = imported_value,
      cwd = getwd(),
      env_value = Sys.getenv("TASKR_TEST_ENV"),
      stats_loaded = "package:stats" %in% search()
    )),
    imports = list(imported_value = 11L),
    packages = "stats",
    workdir = tmp_dir,
    env = list(TASKR_TEST_ENV = "from-test")
  )

  eval(wrapped_expr)
  result <- get(".__taskr_result", envir = environment())
  expected_dir <- normalizePath(tmp_dir, winslash = "/", mustWork = TRUE)

  expect_equal(result$imported_value, 11L)
  expect_identical(result$cwd, expected_dir)
  expect_identical(result$env_value, "from-test")
  expect_true(result$stats_loaded)

  rm(".__taskr_result", envir = environment())
})

test_that("resolve_task_imports returns named objects from .GlobalEnv", {
  assign("taskr_import_test_value", 99L, envir = .GlobalEnv)
  on.exit(rm("taskr_import_test_value", envir = .GlobalEnv), add = TRUE)

  imports <- taskr:::resolve_task_imports("taskr_import_test_value")

  expect_equal(imports$taskr_import_test_value, 99L)
  expect_identical(names(imports), "taskr_import_test_value")
})

test_that("resolve_task_imports validates requested names", {
  expect_error(taskr:::resolve_task_imports(""), "character vector of non-empty object names")
  expect_error(taskr:::resolve_task_imports("taskr_missing_name"), "not found in `.GlobalEnv`")
})

test_that("start_task_process validates that callr is available", {
  if (requireNamespace("callr", quietly = TRUE)) {
    skip("callr is installed; this test targets the missing-dependency branch.")
  }

  expect_error(
    taskr:::start_task_process(id = "task_100", expr = quote(1 + 1)),
    "callr"
  )
})

wait_for_file <- function(path, timeout = 5) {
  deadline <- Sys.time() + timeout

  while (!file.exists(path) && Sys.time() < deadline) {
    Sys.sleep(0.05)
  }

  file.exists(path)
}

test_that("start_task_process launches a background task when callr is installed", {
  skip_if_not_installed("callr")

  result_path <- tempfile(fileext = ".rds")
  task <- taskr:::start_task_process(
    id = "task_101",
    expr = quote({ Sys.sleep(0.2); 7L }),
    result_path = result_path
  )

  expect_equal(task$status(), "running")
  expect_true(task$is_alive())
  expect_true(wait_for_file(result_path))
  expect_equal(readRDS(result_path), 7L)

  task$process$close()
  unlink(result_path)
})

test_that("start_task_process supports import, packages, workdir, and env", {
  skip_if_not_installed("callr")

  assign("taskr_child_import_value", 21L, envir = .GlobalEnv)
  on.exit(rm("taskr_child_import_value", envir = .GlobalEnv), add = TRUE)

  tmp_dir <- tempfile(pattern = "taskr-child-wd-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  result_path <- tempfile(fileext = ".rds")
  task <- taskr:::start_task_process(
    id = "task_105",
    expr = quote(list(
      imported_value = taskr_child_import_value,
      cwd = getwd(),
      env_value = Sys.getenv("TASKR_CHILD_ENV"),
      stats_loaded = "package:stats" %in% search()
    )),
    result_path = result_path,
    import = "taskr_child_import_value",
    packages = "stats",
    workdir = tmp_dir,
    env = list(TASKR_CHILD_ENV = "child-value")
  )

  expect_true(wait_for_file(result_path))
  result <- readRDS(result_path)
  expected_dir <- normalizePath(tmp_dir, winslash = "/", mustWork = TRUE)

  expect_equal(result$imported_value, 21L)
  expect_identical(result$cwd, expected_dir)
  expect_identical(result$env_value, "child-value")
  expect_true(result$stats_loaded)

  task$process$close()
  unlink(result_path)
})

test_that("start_task_process uses task_tmpfile when result_path is omitted", {
  skip_if_not_installed("callr")

  result_path <- taskr:::task_tmpfile("task_102")
  unlink(result_path)

  task <- taskr:::start_task_process(
    id = "task_102",
    expr = quote("saved in default path")
  )

  expect_true(wait_for_file(result_path))
  expect_equal(readRDS(result_path), "saved in default path")

  task$process$close()
  unlink(result_path)
})

test_that("start_task_process leaves no result file when the child errors", {
  skip_if_not_installed("callr")

  result_path <- tempfile(fileext = ".rds")
  task <- taskr:::start_task_process(
    id = "task_103",
    expr = quote(stop("boom from child")),
    result_path = result_path
  )

  Sys.sleep(0.5)
  expect_false(file.exists(result_path))

  task$process$close()
})

test_that("Task kill stops a running child process before it writes a result", {
  skip_if_not_installed("callr")

  result_path <- tempfile(fileext = ".rds")
  task <- taskr:::start_task_process(
    id = "task_104",
    expr = quote({ Sys.sleep(5); "too late" }),
    result_path = result_path
  )

  Sys.sleep(0.1)
  task$kill()

  expect_false(task$is_alive())
  expect_equal(task$status(), "killed")
  expect_false(file.exists(result_path))

  task$process$close()
})
