test_that("dashboard source path detection accepts only taskr packages", {
  dashboard_is_taskr_source_path <- getFromNamespace("dashboard_is_taskr_source_path", "taskr")

  taskr_path <- tempfile("taskr_src_")
  dir.create(taskr_path)
  dir.create(file.path(taskr_path, "R"))
  on.exit(unlink(taskr_path, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c(
      "Package: taskr",
      "Title: Taskr",
      "Version: 0.0.1"
    ),
    file.path(taskr_path, "DESCRIPTION")
  )
  writeLines("demo <- function() TRUE", file.path(taskr_path, "R", "demo.R"))

  expect_true(dashboard_is_taskr_source_path(taskr_path))

  other_path <- tempfile("not_taskr_")
  dir.create(other_path)
  on.exit(unlink(other_path, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c(
      "Package: otherpkg",
      "Title: Not Taskr",
      "Version: 0.0.1"
    ),
    file.path(other_path, "DESCRIPTION")
  )

  expect_false(dashboard_is_taskr_source_path(other_path))
  expect_false(dashboard_is_taskr_source_path(file.path(other_path, "missing")))
})

test_that("dashboard source path detection ignores non-taskr working directories", {
  dashboard_is_taskr_source_path <- getFromNamespace("dashboard_is_taskr_source_path", "taskr")
  dashboard_detect_pkg_path <- getFromNamespace("dashboard_detect_pkg_path", "taskr")

  old_wd <- getwd()
  other_path <- tempfile("not_taskr_wd_")
  dir.create(other_path)
  on.exit({
    setwd(old_wd)
    unlink(other_path, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  writeLines(
    c(
      "Package: otherpkg",
      "Title: Not Taskr",
      "Version: 0.0.1"
    ),
    file.path(other_path, "DESCRIPTION")
  )
  setwd(other_path)

  detected <- dashboard_detect_pkg_path()

  if (nzchar(detected)) {
    expect_true(dashboard_is_taskr_source_path(detected))
  }
  expect_false(identical(normalizePath(other_path, winslash = "/", mustWork = TRUE), detected))
})
