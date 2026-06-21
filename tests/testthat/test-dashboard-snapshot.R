test_that("dashboard snapshot round-trip keeps task rows readable", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")
  write_dashboard_snapshot <- getFromNamespace("write_dashboard_snapshot", "taskr")
  read_dashboard_snapshot <- getFromNamespace("read_dashboard_snapshot", "taskr")

  old_scheduler <- pkg_env$scheduler
  old_snapshot_path <- pkg_env$dashboard_snapshot_path
  on.exit({
    pkg_env$scheduler <- old_scheduler
    pkg_env$dashboard_snapshot_path <- old_snapshot_path
  }, add = TRUE)

  pkg_env$dashboard_snapshot_path <- tempfile(fileext = ".json")
  now <- as.POSIXct("2026-04-11 00:00:00", tz = "UTC")

  item <- list(
    id = 1L,
    label = "demo_001",
    status = "running",
    priority = 1L,
    resources = list(slots = 1L),
    progress = 0.4,
    message = "warmup",
    error = "",
    submit_time = now - 20,
    start_time = now - 10,
    end_time = as.POSIXct(NA),
    stdout_buffer = "Chain 1 Iteration: 400 / 1000 [ 40%] (Warmup)\n",
    stderr_buffer = ""
  )

  state <- new_scheduler_state(max_slots = 3L)
  state$running <- list("1" = item)
  pkg_env$scheduler <- state

  write_dashboard_snapshot(now = now)
  snap <- read_dashboard_snapshot()

  expect_true(is.character(snap$session_id))
  expect_true(length(snap$session_id) == 1)
  expect_equal(snap$max_slots, 3L)
  expect_true(is.data.frame(snap$tasks))
  expect_equal(nrow(snap$tasks), 1)
  expect_identical(snap$tasks$id[[1]], 1L)
  expect_identical(snap$tasks$status[[1]], "running")
  expect_s3_class(snap$tasks$submit_time, "POSIXct")
  expect_s3_class(snap$tasks$start_time, "POSIXct")
})

test_that("dashboard snapshot round-trip preserves local elapsed timing", {
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")
  write_dashboard_snapshot <- getFromNamespace("write_dashboard_snapshot", "taskr")
  read_dashboard_snapshot <- getFromNamespace("read_dashboard_snapshot", "taskr")

  old_scheduler <- pkg_env$scheduler
  old_snapshot_path <- pkg_env$dashboard_snapshot_path
  on.exit({
    pkg_env$scheduler <- old_scheduler
    pkg_env$dashboard_snapshot_path <- old_snapshot_path
  }, add = TRUE)

  pkg_env$dashboard_snapshot_path <- tempfile(fileext = ".json")
  local_tz <- Sys.timezone()
  if (is.null(local_tz) || length(local_tz) == 0 || is.na(local_tz) || !nzchar(local_tz)) {
    local_tz <- ""
  }
  now <- as.POSIXct("2026-04-11 00:00:00", tz = local_tz)

  item <- list(
    id = 1L,
    label = "demo_local",
    status = "running",
    priority = 1L,
    resources = list(slots = 1L),
    progress = 0.4,
    message = "running",
    error = "",
    submit_time = now - 20,
    start_time = now - 10,
    end_time = as.POSIXct(NA),
    stdout_buffer = "",
    stderr_buffer = ""
  )

  state <- new_scheduler_state(max_slots = 1L)
  state$running <- list("1" = item)
  pkg_env$scheduler <- state

  write_dashboard_snapshot(now = now)
  snap <- read_dashboard_snapshot()
  tab <- getFromNamespace("add_dashboard_derived_columns", "taskr")(snap$tasks, now = now)

  expect_equal(tab$running_elapsed_sec[[1]], 10, tolerance = 1e-6)
})
