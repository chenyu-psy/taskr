# Dashboard Data Helpers (Internal)
#
# Purpose:
# - Prepare task snapshot data for the Shiny dashboard.
# - Keep sorting, filtering, and display formatting logic in one place so
#   the dashboard server code stays small and easy to read.

# Create an empty dashboard table with a stable column schema.
# Returns:
# - Zero-row data.frame used as a safe fallback in UI rendering.
empty_dashboard_table <- function() {
  data.frame(
    id = character(),
    label = character(),
    status = character(),
    priority = integer(),
    progress = numeric(),
    message = character(),
    error = character(),
    submit_time = as.POSIXct(character()),
    start_time = as.POSIXct(character()),
    end_time = as.POSIXct(character()),
    stdout = character(),
    stderr = character(),
    stringsAsFactors = FALSE
  )
}

# Format elapsed seconds for compact display in dashboard cards.
# Args:
# - seconds: Numeric scalar elapsed time in seconds.
# Returns:
# - A short human-readable duration string, or "-" for missing values.
format_dashboard_duration <- function(seconds) {
  if (is.null(seconds) || length(seconds) == 0 || is.na(seconds) || !is.finite(seconds)) {
    return("-")
  }

  total <- as.integer(max(0, round(seconds)))
  h <- total %/% 3600L
  m <- (total %% 3600L) %/% 60L
  s <- total %% 60L

  if (h > 0L) {
    return(sprintf("%dh %02dm %02ds", h, m, s))
  }
  if (m > 0L) {
    return(sprintf("%dm %02ds", m, s))
  }

  sprintf("%ds", s)
}

# Format one POSIXct value as "MM-DD HH:MM:SS" for dashboard labels.
# Args:
# - x: POSIXct scalar (or missing value).
# Returns:
# - Formatted time string, or "-" for missing values.
format_dashboard_time <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("-")
  }

  format(as.POSIXct(x), "%m-%d %H:%M:%S")
}

# Collect queue/running/done task items into one flat list.
# Args:
# - state: Scheduler state list.
# Returns:
# - List of task item lists.
collect_dashboard_items <- function(state) {
  c(state$queue %||% list(), unname(state$running %||% list()), unname(state$done %||% list()))
}

# Fallback to task id when label is empty so cards always have a title.
# Args:
# - label: Optional task label.
# - id: Task id.
# Returns:
# - A non-empty card title string.
coalesce_label <- function(label, id) {
  if (is.null(label) || length(label) == 0 || is.na(label) || !nzchar(label)) {
    return(id)
  }
  label
}

# Convert one scheduler task item to a single dashboard table row.
# Args:
# - item: Task item list from scheduler state.
# Returns:
# - One-row data.frame with raw fields used by dashboard rendering.
dashboard_item_to_row <- function(item) {
  data.frame(
    id = as.character(item$id %||% NA_character_),
    label = as.character(coalesce_label(item$label %||% NA_character_, item$id %||% NA_character_)),
    status = as.character(item$status %||% NA_character_),
    priority = as.integer(item$priority %||% 0L),
    progress = as.numeric(item$progress %||% NA_real_),
    message = as.character(item$message %||% ""),
    error = as.character(item$error %||% ""),
    submit_time = as.POSIXct(item$submit_time %||% NA),
    start_time = as.POSIXct(item$start_time %||% NA),
    end_time = as.POSIXct(item$end_time %||% NA),
    stdout = as.character(item$stdout_buffer %||% ""),
    stderr = as.character(item$stderr_buffer %||% ""),
    stringsAsFactors = FALSE
  )
}

# Build one snapshot table from current scheduler state.
# Args:
# - now: Timestamp used to recycle running tasks.
# Returns:
# - Task snapshot data.frame for all queue buckets.
extract_dashboard_snapshot <- function(now = Sys.time()) {
  if (is.null(pkg_env$scheduler)) {
    return(empty_dashboard_table())
  }

  pkg_env$scheduler <- recycle_running_tasks(pkg_env$scheduler, now = now)
  if (scheduler_has_work(pkg_env$scheduler)) {
    start_scheduler_internal()
  }

  items <- collect_dashboard_items(pkg_env$scheduler)
  if (length(items) == 0) {
    return(empty_dashboard_table())
  }

  rows <- lapply(items, dashboard_item_to_row)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Build an event-style signature for queue structure changes.
# Args:
# - tab: Snapshot table from `extract_dashboard_snapshot()`.
# Returns:
# - Character scalar used by `reactivePoll(checkFunc=...)`.
dashboard_state_signature <- function(tab) {
  if (nrow(tab) == 0) {
    return("state:empty")
  }

  key <- data.frame(
    id = as.character(tab$id),
    status = as.character(tab$status),
    priority = as.integer(tab$priority),
    submit_time = as.numeric(tab$submit_time),
    start_time = as.numeric(tab$start_time),
    end_time = as.numeric(tab$end_time),
    stringsAsFactors = FALSE
  )
  key <- key[order(key$id), , drop = FALSE]

  paste(
    "state",
    paste(apply(key, 1, function(row) paste(row, collapse = "|")), collapse = ";"),
    sep = ":"
  )
}

# Build an event-style signature for running-task content updates.
# Args:
# - tab: Snapshot table from `extract_dashboard_snapshot()`.
# Returns:
# - Character scalar used by `reactivePoll(checkFunc=...)`.
dashboard_running_signature <- function(tab) {
  running <- tab[tab$status == "running", , drop = FALSE]
  if (nrow(running) == 0) {
    return("running:empty")
  }

  key <- data.frame(
    id = as.character(running$id),
    progress = as.character(running$progress),
    message = as.character(running$message),
    stdout_nchar = nchar(as.character(running$stdout %||% ""), type = "bytes", allowNA = FALSE, keepNA = FALSE),
    stderr_nchar = nchar(as.character(running$stderr %||% ""), type = "bytes", allowNA = FALSE, keepNA = FALSE),
    stringsAsFactors = FALSE
  )
  key <- key[order(key$id), , drop = FALSE]

  paste(
    "running",
    paste(apply(key, 1, function(row) paste(row, collapse = "|")), collapse = ";"),
    sep = ":"
  )
}

# Add derived display columns (elapsed/wait/time labels) for dashboard UI.
# Args:
# - tab: Raw snapshot table from `extract_dashboard_snapshot()`.
# - now: Timestamp used to compute elapsed durations.
# Returns:
# - Input table with additional derived columns.
add_dashboard_derived_columns <- function(tab, now = Sys.time()) {
  if (nrow(tab) == 0) {
    tab$running_elapsed_sec <- numeric()
    tab$queue_wait_sec <- numeric()
    tab$running_elapsed <- character()
    tab$queue_wait <- character()
    tab$submit_time_label <- character()
    tab$start_time_label <- character()
    tab$end_time_label <- character()
    return(tab)
  }

  tab$running_elapsed_sec <- as.numeric(difftime(now, tab$start_time, units = "secs"))
  tab$queue_wait_sec <- as.numeric(difftime(now, tab$submit_time, units = "secs"))

  tab$running_elapsed <- vapply(tab$running_elapsed_sec, format_dashboard_duration, character(1))
  tab$queue_wait <- vapply(tab$queue_wait_sec, format_dashboard_duration, character(1))
  tab$submit_time_label <- vapply(tab$submit_time, format_dashboard_time, character(1))
  tab$start_time_label <- vapply(tab$start_time, format_dashboard_time, character(1))
  tab$end_time_label <- vapply(tab$end_time, format_dashboard_time, character(1))
  tab
}

# Filter dashboard table by id/label query string.
# Args:
# - tab: Dashboard table.
# - query: Free-text search string.
# Returns:
# - Filtered dashboard table.
filter_dashboard_tasks <- function(tab, query = "") {
  if (nrow(tab) == 0) {
    return(tab)
  }

  q <- trimws(tolower(query %||% ""))
  if (!nzchar(q)) {
    return(tab)
  }

  keep <- grepl(q, tolower(tab$id), fixed = TRUE) |
    grepl(q, tolower(tab$label), fixed = TRUE)
  tab[keep, , drop = FALSE]
}

# Split dashboard table into running/queued/done views with requested sorting.
# Args:
# - tab: Dashboard table.
# Returns:
# - Named list with data.frames: running, queued, done.
split_dashboard_tasks <- function(tab) {
  if (nrow(tab) == 0) {
    return(list(
      running = tab,
      queued = tab,
      done = tab
    ))
  }

  running <- tab[tab$status == "running", , drop = FALSE]
  queued <- tab[tab$status == "queued", , drop = FALSE]
  done <- tab[tab$status %in% c("done", "failed", "killed"), , drop = FALSE]

  if (nrow(running) > 0) {
    running <- running[order(running$start_time, decreasing = TRUE), , drop = FALSE]
  }

  if (nrow(queued) > 0) {
    queued <- queued[order(-queued$priority, queued$submit_time), , drop = FALSE]
  }

  if (nrow(done) > 0) {
    done <- done[order(done$end_time, decreasing = TRUE), , drop = FALSE]
  }

  list(running = running, queued = queued, done = done)
}

# Compute summary counters and progress ratios for top summary panel.
# Args:
# - tab: Dashboard table.
# - max_slots: Queue slot capacity.
# Returns:
# - Named list with counts and ratio values.
dashboard_summary_metrics <- function(tab, max_slots = 1L) {
  n_running <- sum(tab$status == "running")
  n_queued <- sum(tab$status == "queued")
  n_done <- sum(tab$status == "done")
  n_failed <- sum(tab$status == "failed")
  n_killed <- sum(tab$status == "killed")
  n_total <- nrow(tab)

  terminal_count <- n_done + n_failed + n_killed
  completion_ratio <- if (n_total == 0) 0 else terminal_count / n_total

  slots <- max(1L, as.integer(max_slots %||% 1L))
  slot_ratio <- min(1, n_running / slots)

  list(
    total = n_total,
    running = n_running,
    queued = n_queued,
    done = n_done,
    failed = n_failed,
    killed = n_killed,
    slots_used = n_running,
    slots_total = slots,
    slot_ratio = slot_ratio,
    completion_ratio = completion_ratio
  )
}

# Map task status to CSS class for status pill rendering.
# Args:
# - status: Task status string.
# Returns:
# - CSS class string.
status_badge_class <- function(status) {
  switch(
    status,
    running = "status-running",
    queued = "status-queued",
    done = "status-done",
    failed = "status-failed",
    killed = "status-killed",
    "status-unknown"
  )
}

# Map task status to card wrapper CSS class.
# Args:
# - status: Task status string.
# Returns:
# - CSS class string.
dashboard_card_class <- function(status) {
  if (identical(status, "failed")) {
    return("task-card task-card-failed")
  }

  "task-card"
}

# Build deterministic UI button ids from task ids.
# Args:
# - prefix: Button type prefix (e.g., "select", "cancel").
# - task_id: Original task id.
# Returns:
# - Sanitized UI id string.
button_id_for_task <- function(prefix, task_id) {
  safe_id <- gsub("[^A-Za-z0-9_]", "_", task_id)
  paste0(prefix, "_", safe_id)
}

# Build combined stdout/stderr tail text for details panel.
# Args:
# - task_id: Target task id.
# - tail_n: Maximum number of lines kept per stream.
# Returns:
# - Multi-line text block for verbatim display.
dashboard_log_text <- function(task_id, tail_n = 200L) {
  if (is.null(task_id) || length(task_id) == 0 || is.na(task_id) || !nzchar(task_id)) {
    return("Select one task card to view logs.")
  }

  logs <- tryCatch(task_logs(task_id), error = function(e) NULL)
  if (is.null(logs)) {
    return("Task logs are not available for the selected task.")
  }

  stdout_lines <- unlist(strsplit(logs$stdout %||% "", "\n", fixed = TRUE), use.names = FALSE)
  stderr_lines <- unlist(strsplit(logs$stderr %||% "", "\n", fixed = TRUE), use.names = FALSE)

  if (length(stdout_lines) > tail_n) {
    stdout_lines <- tail(stdout_lines, tail_n)
  }
  if (length(stderr_lines) > tail_n) {
    stderr_lines <- tail(stderr_lines, tail_n)
  }

  stdout_text <- if (length(stdout_lines) == 0) "(empty)" else paste(stdout_lines, collapse = "\n")
  stderr_text <- if (length(stderr_lines) == 0) "(empty)" else paste(stderr_lines, collapse = "\n")

  paste(
    "=== STDOUT (tail) ===",
    stdout_text,
    "",
    "=== STDERR (tail) ===",
    stderr_text,
    sep = "\n"
  )
}

# Resolve selected task row, with running-first fallback for first load.
# Args:
# - tab: Dashboard table.
# - selected_id: Optional selected task id.
# Returns:
# - One-row data.frame, or NULL when no tasks are available.
selected_task_row <- function(tab, selected_id = NULL) {
  if (nrow(tab) == 0) {
    return(NULL)
  }

  if (!is.null(selected_id) && selected_id %in% tab$id) {
    return(tab[tab$id == selected_id, , drop = FALSE][1, , drop = FALSE])
  }

  running <- tab[tab$status == "running", , drop = FALSE]
  if (nrow(running) > 0) {
    return(running[1, , drop = FALSE])
  }

  tab[1, , drop = FALSE]
}

# Normalize one progress value into [0, 1] for dashboard bars.
# Args:
# - progress: Raw progress value from task state.
# - default_fraction: Fallback used when progress is missing.
# Returns:
# - Numeric scalar in [0, 1].
normalize_progress_fraction <- function(progress, default_fraction = 0.5) {
  p <- as.numeric(progress)
  if (is.null(p) || length(p) == 0 || is.na(p) || !is.finite(p)) {
    return(max(0, min(1, as.numeric(default_fraction))))
  }

  max(0, min(1, p[[1]]))
}

# Parse per-chain Stan/model progress from one task's logs.
# Args:
# - task_id: Task id used by `task_logs()`.
# Returns:
# - data.frame with columns `chain`, `progress`, and `phase`.
dashboard_stan_chain_progress <- function(task_id) {
  if (is.null(task_id) || length(task_id) == 0 || is.na(task_id) || !nzchar(task_id)) {
    return(data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      stringsAsFactors = FALSE
    ))
  }

  logs <- tryCatch(task_logs(task_id), error = function(e) NULL)
  if (is.null(logs)) {
    return(data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      stringsAsFactors = FALSE
    ))
  }

  text <- paste(logs$stdout %||% "", logs$stderr %||% "", sep = "\n")
  tab <- extract_stan_progress_rows(text)
  if (nrow(tab) == 0) {
    return(data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      stringsAsFactors = FALSE
    ))
  }

  tab[, c("chain", "progress", "phase"), drop = FALSE]
}
