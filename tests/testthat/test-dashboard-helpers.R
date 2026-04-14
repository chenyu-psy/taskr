make_dashboard_item <- function(id, label, status, priority = 0L, submit_time, start_time = NA, end_time = NA) {
  list(
    id = id,
    label = label,
    status = status,
    priority = priority,
    resources = list(slots = 1L),
    progress = 0.5,
    message = "msg",
    error = if (status == "failed") "boom" else "",
    submit_time = submit_time,
    start_time = start_time,
    end_time = end_time,
    stdout_buffer = "",
    stderr_buffer = ""
  )
}

fake_stan_printer_lines <- function(steps = 8L, chains = 4L) {
  out <- character()
  for (s in seq_len(as.integer(steps))) {
    chain_id <- ((s - 1L) %% as.integer(chains)) + 1L
    iter <- as.integer(round(s / steps * 1000))
    pct <- as.integer(round(s / steps * 100))
    phase <- if (pct < 50L) "Warmup" else "Sampling"
    out[[length(out) + 1L]] <- sprintf(
      "Chain %d Iteration: %d / 1000 [%3d%%] (%s)",
      chain_id,
      iter,
      pct,
      phase
    )
  }
  out
}

test_that("split_dashboard_tasks applies requested sorting rules", {
  split_dashboard_tasks <- getFromNamespace("split_dashboard_tasks", "taskr")

  now <- as.POSIXct("2026-04-10 12:00:00", tz = "UTC")
  tab <- data.frame(
    id = c("r_old", "r_new", "q_low_old", "q_high_new", "d_old", "f_new"),
    label = c("r_old", "r_new", "q_low_old", "q_high_new", "d_old", "f_new"),
    status = c("running", "running", "queued", "queued", "completed", "failed"),
    priority = c(0L, 0L, 1L, 3L, 0L, 0L),
    submit_time = c(now - 60, now - 30, now - 300, now - 100, now - 500, now - 400),
    start_time = c(now - 100, now - 20, NA, NA, now - 200, now - 150),
    end_time = c(NA, NA, NA, NA, now - 40, now - 10),
    stringsAsFactors = FALSE
  )

  out <- split_dashboard_tasks(tab)

  expect_identical(out$running$id, c("r_new", "r_old"))
  expect_identical(out$queued$id, c("q_high_new", "q_low_old"))
  expect_identical(out$finished$id, c("f_new", "d_old"))
})

test_that("dashboard_summary_metrics returns slot and completion ratios", {
  dashboard_summary_metrics <- getFromNamespace("dashboard_summary_metrics", "taskr")

  tab <- data.frame(
    id = paste0("task_", 1:5),
    status = c("running", "queued", "completed", "failed", "cancelled"),
    slots = c(3L, 1L, 1L, 1L, 1L),
    stringsAsFactors = FALSE
  )

  summary <- dashboard_summary_metrics(tab, max_slots = 4L)

  expect_equal(summary$total, 5)
  expect_equal(summary$running, 1)
  expect_equal(summary$queued, 1)
  expect_equal(summary$completed, 1)
  expect_equal(summary$failed, 1)
  expect_equal(summary$cancelled, 1)
  expect_equal(summary$slots_used, 3)
  expect_equal(summary$slots_total, 4)
  expect_equal(summary$slot_ratio, 0.75)
  expect_equal(summary$completion_ratio, 3 / 5)
})

test_that("dashboard_summary_metrics falls back to running count when slots column is absent", {
  dashboard_summary_metrics <- getFromNamespace("dashboard_summary_metrics", "taskr")

  tab <- data.frame(
    id = c("task_001", "task_002"),
    status = c("running", "queued"),
    stringsAsFactors = FALSE
  )

  summary <- dashboard_summary_metrics(tab, max_slots = 4L)

  expect_equal(summary$slots_used, 1)
  expect_equal(summary$slot_ratio, 0.25)
})

test_that("filter_dashboard_tasks matches id and label case-insensitively", {
  filter_dashboard_tasks <- getFromNamespace("filter_dashboard_tasks", "taskr")

  tab <- data.frame(
    id = c("task_001", "task_002"),
    label = c("Fit_Main", "pilot"),
    stringsAsFactors = FALSE
  )

  out_a <- filter_dashboard_tasks(tab, query = "fit")
  out_b <- filter_dashboard_tasks(tab, query = "002")

  expect_identical(out_a$id, "task_001")
  expect_identical(out_b$id, "task_002")
})

test_that("dashboard_log_text returns readable output/error tail blocks", {
  dashboard_log_text <- getFromNamespace("dashboard_log_text", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  item <- make_dashboard_item(
    id = "task_log_001",
    label = "log_demo",
    status = "completed",
    submit_time = Sys.time() - 10,
    end_time = Sys.time() - 1
  )
  item$stdout_buffer <- paste0("line_1\nline_2\nline_3\nline_4")
  item$stderr_buffer <- paste0("err_1\nerr_2")

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(task_log_001 = item)

  txt <- dashboard_log_text("task_log_001", tail_n = 2)

  expect_match(txt, "=== Output \\(latest lines\\) ===")
  expect_match(txt, "line_3")
  expect_match(txt, "line_4")
  expect_false(grepl("line_1", txt, fixed = TRUE))
  expect_match(txt, "=== Messages & Errors \\(latest lines\\) ===")
  expect_match(txt, "err_1")
  expect_match(txt, "err_2")
})

test_that("normalize_progress_fraction uses 0.5 for missing progress", {
  normalize_progress_fraction <- getFromNamespace("normalize_progress_fraction", "taskr")

  expect_equal(normalize_progress_fraction(NA_real_), 0.5)
  expect_equal(normalize_progress_fraction(NULL), 0.5)
  expect_equal(normalize_progress_fraction(0.2), 0.2)
  expect_equal(normalize_progress_fraction(2), 1)
  expect_equal(normalize_progress_fraction(-1), 0)
})

test_that("dashboard_state_signature ignores running progress/message-only changes", {
  dashboard_state_signature <- getFromNamespace("dashboard_state_signature", "taskr")

  now <- as.POSIXct("2026-04-10 12:00:00", tz = "UTC")
  tab_a <- data.frame(
    id = "task_001",
    label = "demo",
    status = "running",
    priority = 1L,
    progress = 0.1,
    message = "phase A",
    error = "",
    submit_time = now - 20,
    start_time = now - 10,
    end_time = as.POSIXct(NA),
    stdout = "log_1",
    stderr = "",
    stringsAsFactors = FALSE
  )
  tab_b <- tab_a
  tab_b$progress <- 0.9
  tab_b$message <- "phase B"
  tab_b$stdout <- "log_2"

  expect_identical(
    dashboard_state_signature(tab_a),
    dashboard_state_signature(tab_b)
  )
})

test_that("dashboard_running_signature reacts to progress, message, and log changes", {
  dashboard_running_signature <- getFromNamespace("dashboard_running_signature", "taskr")

  now <- as.POSIXct("2026-04-10 12:00:00", tz = "UTC")
  tab_a <- data.frame(
    id = "task_001",
    label = "demo",
    status = "running",
    priority = 1L,
    progress = 0.1,
    message = "phase A",
    error = "",
    submit_time = now - 20,
    start_time = now - 10,
    end_time = as.POSIXct(NA),
    stdout = "log_1",
    stderr = "",
    stringsAsFactors = FALSE
  )
  tab_b <- tab_a
  tab_b$progress <- 0.9
  tab_b$message <- "phase B"

  expect_false(identical(
    dashboard_running_signature(tab_a),
    dashboard_running_signature(tab_b)
  ))

  tab_c <- tab_a
  tab_c$stdout <- "log_2_more"
  tab_c$stderr <- "warn"

  expect_false(identical(
    dashboard_running_signature(tab_a),
    dashboard_running_signature(tab_c)
  ))
})

test_that("dashboard_running_signature reacts when log progress changes", {
  dashboard_running_signature <- getFromNamespace("dashboard_running_signature", "taskr")

  now <- as.POSIXct("2026-04-10 12:00:00", tz = "UTC")
  tab_a <- data.frame(
    id = "task_001",
    label = "demo",
    status = "running",
    priority = 1L,
    progress = NA_real_,
    message = "",
    error = "",
    submit_time = now - 20,
    start_time = now - 10,
    end_time = as.POSIXct(NA),
    stdout = "Step 1 / 100",
    stderr = "",
    stringsAsFactors = FALSE
  )
  tab_b <- tab_a
  tab_b$stdout <- "Step 50 / 100"

  expect_false(identical(
    dashboard_running_signature(tab_a),
    dashboard_running_signature(tab_b)
  ))
})

test_that("dashboard_running_log_signature reacts to running log changes", {
  dashboard_running_log_signature <- getFromNamespace("dashboard_running_log_signature", "taskr")

  now <- as.POSIXct("2026-04-10 12:00:00", tz = "UTC")
  tab_a <- data.frame(
    id = "task_001",
    label = "demo",
    status = "running",
    priority = 1L,
    progress = 0.1,
    message = "phase A",
    error = "",
    submit_time = now - 20,
    start_time = now - 10,
    end_time = as.POSIXct(NA),
    stdout = "log_1",
    stderr = "",
    stringsAsFactors = FALSE
  )
  tab_b <- tab_a
  tab_b$stdout <- "log_2_more"
  tab_b$stderr <- "warn"

  expect_false(identical(
    dashboard_running_log_signature(tab_a, task_id = "task_001"),
    dashboard_running_log_signature(tab_b, task_id = "task_001")
  ))
})

test_that("dashboard_stan_chain_progress parses chain rows from logs", {
  dashboard_stan_chain_progress <- getFromNamespace("dashboard_stan_chain_progress", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  item <- make_dashboard_item(
    id = "task_chain_001",
    label = "model_demo",
    status = "running",
    submit_time = Sys.time() - 10,
    start_time = Sys.time() - 5
  )
  item$stdout_buffer <- paste(
    "Chain 1 Iteration: 200 / 1000 [ 20%] (Warmup)",
    "Chain 2 Iteration: 500 / 1000 [ 50%] (Sampling)",
    sep = "\n"
  )

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$running <- list(task_chain_001 = item)

  tab <- dashboard_stan_chain_progress("task_chain_001")

  expect_equal(nrow(tab), 2)
  expect_identical(tab$chain, c(1L, 2L))
  expect_equal(round(tab$progress, 2), c(0.20, 0.50))
})

test_that("fake stan printer output yields one latest row per chain", {
  extract_stan_progress_rows <- getFromNamespace("extract_stan_progress_rows", "taskr")

  text <- paste(fake_stan_printer_lines(steps = 16L, chains = 4L), collapse = "\n")
  tab <- extract_stan_progress_rows(text)

  expect_equal(nrow(tab), 4)
  expect_identical(tab$chain, c(1L, 2L, 3L, 4L))
  expect_true(all(tab$progress >= 0 & tab$progress <= 1))
})

test_that("dashboard_stan_chain_progress sees logs consumed by task status updates", {
  dashboard_stan_chain_progress <- getFromNamespace("dashboard_stan_chain_progress", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")
  recycle_running_tasks <- getFromNamespace("recycle_running_tasks", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  fake_task <- new.env(parent = emptyenv())
  fake_task$stdout_buffer <- ""
  fake_task$stderr_buffer <- ""
  fake_task$progress_state <- NULL
  fake_task$read_output <- function() ""
  fake_task$read_error <- function() ""
  fake_task$progress <- function() fake_task$progress_state
  fake_task$status <- function() {
    # Simulate status() pulling process events and appending internal buffers.
    fake_task$stderr_buffer <- paste(
      fake_task$stderr_buffer,
      "Chain 1 Iteration: 500 / 1000 [ 50%] (Warmup)\n",
      "Chain 2 Iteration: 900 / 1000 [ 90%] (Sampling)\n",
      sep = ""
    )
    "running"
  }
  fake_task$error <- NULL

  item <- make_dashboard_item(
    id = "task_sync_001",
    label = "sync_demo",
    status = "running",
    submit_time = Sys.time() - 10,
    start_time = Sys.time() - 5
  )
  item$task <- fake_task

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$running <- list(task_sync_001 = item)
  pkg_env$scheduler <- recycle_running_tasks(pkg_env$scheduler, now = Sys.time())

  tab <- dashboard_stan_chain_progress("task_sync_001")
  expect_equal(nrow(tab), 2)
  expect_identical(tab$chain, c(1L, 2L))
  expect_equal(round(tab$progress, 2), c(0.50, 0.90))
})

test_that("parse_generic_progress_fraction uses line-level priority", {
  parse_generic_progress_fraction <- getFromNamespace("parse_generic_progress_fraction", "taskr")

  text <- paste(
    "Step 3/10 (45%) [#####-----]",
    "still running",
    sep = "\n"
  )

  expect_equal(parse_generic_progress_fraction(text), 0.3)
})

test_that("parse_generic_progress_fraction ignores non-progress percent contexts", {
  parse_generic_progress_fraction <- getFromNamespace("parse_generic_progress_fraction", "taskr")

  text <- paste(
    "accuracy 45%",
    "memory usage 72%",
    sep = "\n"
  )

  expect_true(is.na(parse_generic_progress_fraction(text)))
})

test_that("extract_chain_progress_rows parses non-stan chain fraction and percent lines", {
  extract_chain_progress_rows <- getFromNamespace("extract_chain_progress_rows", "taskr")

  text <- paste(
    "Chain 1 step 20/100",
    "Chain 2: progress 45%",
    sep = "\n"
  )

  tab <- extract_chain_progress_rows(text)
  expect_equal(nrow(tab), 2)
  expect_identical(tab$chain, c(1L, 2L))
  expect_equal(round(tab$progress, 2), c(0.20, 0.45))
})

test_that("resolve_dashboard_progress_fraction freezes prior progress on ambiguous lines", {
  resolve_dashboard_progress_fraction <- getFromNamespace("resolve_dashboard_progress_fraction", "taskr")

  expect_equal(
    resolve_dashboard_progress_fraction(
      parsed_fraction = NA_real_,
      task_fraction = NA_real_,
      cached_fraction = 0.62
    ),
    0.62
  )

  expect_equal(
    resolve_dashboard_progress_fraction(
      parsed_fraction = NA_real_,
      task_fraction = 0.4,
      cached_fraction = 0.62
    ),
    0.4
  )
})

test_that("resolve_dashboard_progress_fraction uses 1% before fallback timeout", {
  resolve_dashboard_progress_fraction <- getFromNamespace("resolve_dashboard_progress_fraction", "taskr")
  now <- as.POSIXct("2026-04-11 00:00:05", tz = "UTC")

  expect_equal(
    resolve_dashboard_progress_fraction(
      parsed_fraction = NA_real_,
      task_fraction = NA_real_,
      cached_fraction = NA_real_,
      start_time = now - 2,
      now = now
    ),
    0.01
  )
})

test_that("resolve_dashboard_progress_fraction falls back to 50% after timeout", {
  resolve_dashboard_progress_fraction <- getFromNamespace("resolve_dashboard_progress_fraction", "taskr")
  now <- as.POSIXct("2026-04-11 00:00:10", tz = "UTC")

  expect_equal(
    resolve_dashboard_progress_fraction(
      parsed_fraction = NA_real_,
      task_fraction = NA_real_,
      cached_fraction = NA_real_,
      start_time = now - 6,
      now = now,
      fallback_after_sec = 5
    ),
    0.5
  )

  expect_equal(
    resolve_dashboard_progress_fraction(
      parsed_fraction = NA_real_,
      task_fraction = NA_real_,
      cached_fraction = 0.01,
      start_time = now - 6,
      now = now,
      fallback_after_sec = 5
    ),
    0.5
  )
})

test_that("dashboard_initial_progress_wait_sec uses option with sane fallback", {
  dashboard_initial_progress_wait_sec <- getFromNamespace("dashboard_initial_progress_wait_sec", "taskr")

  old_opt <- getOption("taskr.dashboard.initial_progress_wait_sec")
  on.exit(options(taskr.dashboard.initial_progress_wait_sec = old_opt), add = TRUE)

  options(taskr.dashboard.initial_progress_wait_sec = NULL)
  expect_equal(dashboard_initial_progress_wait_sec(), 3)

  options(taskr.dashboard.initial_progress_wait_sec = 5)
  expect_equal(dashboard_initial_progress_wait_sec(), 5)

  options(taskr.dashboard.initial_progress_wait_sec = -1)
  expect_equal(dashboard_initial_progress_wait_sec(), 3)
})

test_that("dashboard_parse_task_progress returns generic parsed fraction when no chain rows", {
  dashboard_parse_task_progress <- getFromNamespace("dashboard_parse_task_progress", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  item <- make_dashboard_item(
    id = "task_generic_001",
    label = "generic_demo",
    status = "running",
    submit_time = Sys.time() - 8,
    start_time = Sys.time() - 5
  )
  item$stdout_buffer <- "Epoch 3/10"

  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$running <- list(task_generic_001 = item)

  out <- dashboard_parse_task_progress("task_generic_001")
  expect_equal(nrow(out$chain), 0)
  expect_equal(out$fraction, 0.3)
})

test_that("normalize_dashboard_posixct keeps POSIXct values and handles missing values", {
  normalize_dashboard_posixct <- getFromNamespace("normalize_dashboard_posixct", "taskr")

  ts <- as.POSIXct("2026-04-11 00:00:00", tz = "UTC")
  out <- normalize_dashboard_posixct(ts)
  expect_s3_class(out, "POSIXct")
  expect_equal(as.numeric(out), as.numeric(ts))

  out_na <- normalize_dashboard_posixct(NA)
  expect_true(is.na(out_na))
})

test_that("running task card hides message row when message is NA", {
  skip_if_not_installed("shiny")

  running_task_card_ui <- getFromNamespace("running_task_card_ui", "taskr")

  task <- data.frame(
    id = "task_001",
    label = "demo",
    status = "running",
    running_elapsed = "5s",
    start_time = as.POSIXct("2026-04-11 00:00:00", tz = "UTC"),
    start_time_label = "04-11 00:00:00",
    message = NA_character_,
    priority = 1L,
    submit_time_label = "04-11 00:00:00",
    end_time_label = "-",
    error = "",
    stringsAsFactors = FALSE
  )

  ui <- running_task_card_ui(
    task = task[1, , drop = FALSE],
    expanded = FALSE,
    chain_tab = data.frame(chain = integer(), progress = numeric(), phase = character(), stringsAsFactors = FALSE),
    progress_ratio = 0.2
  )
  html <- as.character(ui)
  expect_false(grepl(">NA<", html, fixed = TRUE))
})

test_that("task card action buttons do not create duplicate Shiny input ids", {
  skip_if_not_installed("shiny")

  running_task_card_ui <- getFromNamespace("running_task_card_ui", "taskr")

  task <- data.frame(
    id = "task_001",
    label = "demo",
    status = "running",
    running_elapsed = "5s",
    start_time = as.POSIXct("2026-04-11 00:00:00", tz = "UTC"),
    start_time_label = "04-11 00:00:00",
    message = "",
    priority = 1L,
    submit_time_label = "04-11 00:00:00",
    end_time_label = "-",
    error = "",
    stringsAsFactors = FALSE
  )

  ui <- running_task_card_ui(
    task = task[1, , drop = FALSE],
    expanded = FALSE,
    chain_tab = data.frame(chain = integer(), progress = numeric(), phase = character(), stringsAsFactors = FALSE),
    progress_ratio = 0.2
  )
  html <- as.character(ui)

  expect_false(grepl('id="select_task_001"', html, fixed = TRUE))
  expect_false(grepl('id="cancel_task_001"', html, fixed = TRUE))
  expect_true(grepl('data-taskr-action="select"', html, fixed = TRUE))
  expect_true(grepl('data-taskr-action="cancel"', html, fixed = TRUE))
  expect_true(grepl('data-taskr-task-id="task_001"', html, fixed = TRUE))
})

test_that("dashboard custom message handlers use Shiny-compatible signatures", {
  skip_if_not_installed("shiny")

  dashboard_scroll_js <- getFromNamespace("dashboard_scroll_js", "taskr")
  script <- as.character(dashboard_scroll_js())

  expect_true(grepl("addCustomMessageHandler('taskr_update_logs', function (msg)", script, fixed = TRUE))
  expect_true(grepl("addCustomMessageHandler('taskr_focus_task', function (msg)", script, fixed = TRUE))
  expect_true(grepl("addCustomMessageHandler('taskr_force_close_modal', function (msg)", script, fixed = TRUE))
  expect_true(grepl("addCustomMessageHandler('taskr_control_request', function (msg)", script, fixed = TRUE))
})
