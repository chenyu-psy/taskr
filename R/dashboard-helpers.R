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
    slots = integer(),
    priority = integer(),
    progress = numeric(),
    message = character(),
    error = character(),
    submit_time = as.POSIXct(character()),
    start_time = as.POSIXct(character()),
    end_time = as.POSIXct(character()),
    pid = integer(),
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

  local_tz <- Sys.timezone()
  if (is.null(local_tz) || length(local_tz) == 0 || is.na(local_tz) || !nzchar(local_tz)) {
    local_tz <- ""
  }

  format(as.POSIXct(x), "%m-%d %H:%M:%S", tz = local_tz)
}

# Normalize one task timestamp field to POSIXct.
# Keeps existing POSIXct values intact to avoid accidental timezone drift.
normalize_dashboard_posixct <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(as.POSIXct(NA))
  }

  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x))
  }

  suppressWarnings(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
}

# Collect queue/running/finished task items into one flat list.
# Args:
# - state: Scheduler state list.
# Returns:
# - List of task item lists.
collect_dashboard_items <- function(state) {
  c(state$queue %||% list(), unname(state$running %||% list()), unname(state$finished %||% list()))
}

# Build one snapshot table from a provided scheduler state.
# Args:
# - state: Scheduler state list.
# Returns:
# - Snapshot data.frame with stable dashboard columns.
dashboard_snapshot_table_from_state <- function(state) {
  if (is.null(state)) {
    return(empty_dashboard_table())
  }

  items <- collect_dashboard_items(state)
  if (length(items) == 0) {
    return(empty_dashboard_table())
  }

  rows <- lapply(items, dashboard_item_to_row)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
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

# Normalize one task slot declaration for dashboard display math.
# Args:
# - item: Task item list from scheduler state.
# Returns:
# - Integer scalar >= 1. Falls back to 1 when missing/invalid.
dashboard_item_slots <- function(item) {
  slots <- suppressWarnings(as.integer(item$resources$slots %||% 1L))
  if (is.na(slots) || slots < 1L) {
    return(1L)
  }
  slots
}

# Read the operating-system PID for a running task process.
# Args:
# - item: Task item list from scheduler state.
# Returns:
# - Integer PID when available, otherwise NA_integer_.
# Assumptions:
# - Only running callr-backed tasks expose a process with get_pid().
dashboard_item_pid <- function(item) {
  task <- item$task %||% NULL
  process <- if (is.null(task)) NULL else task$process %||% NULL
  if (is.null(process) || is.null(process$get_pid) || !is.function(process$get_pid)) {
    return(NA_integer_)
  }

  pid <- suppressWarnings(as.integer(tryCatch(process$get_pid(), error = function(e) NA_integer_)))
  if (length(pid) != 1 || is.na(pid) || pid < 1L) {
    return(NA_integer_)
  }
  pid
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
    slots = dashboard_item_slots(item),
    priority = as.integer(item$priority %||% 0L),
    progress = as.numeric(item$progress %||% NA_real_),
    message = as.character(item$message %||% ""),
    error = as.character(item$error %||% ""),
    submit_time = normalize_dashboard_posixct(item$submit_time %||% NA),
    start_time = normalize_dashboard_posixct(item$start_time %||% NA),
    end_time = normalize_dashboard_posixct(item$end_time %||% NA),
    pid = dashboard_item_pid(item),
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
    start_scheduler()
  }

  dashboard_snapshot_table_from_state(pkg_env$scheduler)
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

# Build a compact signature for log text that can affect visible progress.
# Args:
# - text: Log text from stdout or stderr.
# - tail_chars: Number of trailing characters to include.
# Returns:
# - Character scalar containing text length and tail text.
# Notes:
# - Running cards parse progress from logs, so log growth must invalidate the
#   card UI. Keeping only the length plus tail avoids putting full logs into
#   the reactive signature.
dashboard_log_progress_signature <- function(text, tail_chars = 500L) {
  text <- paste0(as.character(text %||% ""), collapse = "\n")
  n <- nchar(text, type = "chars", allowNA = FALSE, keepNA = FALSE)
  tail_chars <- max(1L, as.integer(tail_chars %||% 500L))
  start <- max(1L, n - tail_chars + 1L)
  tail_text <- if (n == 0L) "" else substr(text, start, n)
  paste(n, tail_text, sep = ":")
}

# Build an event-style signature for running-card structure/content updates.
# Purpose:
# - Trigger running-card UI updates when progress/message changes.
# - Include a compact stdout/stderr signature because visible progress can be
#   parsed from task logs even when task$progress is not updated.
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
    stdout = vapply(running$stdout, dashboard_log_progress_signature, character(1)),
    stderr = vapply(running$stderr, dashboard_log_progress_signature, character(1)),
    stringsAsFactors = FALSE
  )
  key <- key[order(key$id), , drop = FALSE]

  paste(
    "running",
    paste(apply(key, 1, function(row) paste(row, collapse = "|")), collapse = ";"),
    sep = ":"
  )
}

# Build an event-style signature for one running task's log text updates.
# Purpose:
# - Track only stdout/stderr growth for the currently expanded running card.
# - Let details logs update in place without re-rendering the full card list.
# Args:
# - tab: Snapshot table from `extract_dashboard_snapshot()`.
# - task_id: Character task id currently expanded by the user.
# Returns:
# - Character scalar used by `reactivePoll(checkFunc=...)`.
dashboard_running_log_signature <- function(tab, task_id = NULL) {
  task_id <- as.character(task_id %||% "")
  if (!nzchar(task_id)) {
    return("running-log:none")
  }

  running <- tab[tab$status == "running", , drop = FALSE]
  if (nrow(running) == 0) {
    return(paste("running-log", task_id, "inactive", sep = ":"))
  }

  row <- running[running$id == task_id, , drop = FALSE]
  if (nrow(row) == 0) {
    return(paste("running-log", task_id, "inactive", sep = ":"))
  }

  stdout_nchar <- nchar(as.character(row$stdout[[1]] %||% ""), type = "bytes", allowNA = FALSE, keepNA = FALSE)
  stderr_nchar <- nchar(as.character(row$stderr[[1]] %||% ""), type = "bytes", allowNA = FALSE, keepNA = FALSE)
  paste("running-log", task_id, stdout_nchar, stderr_nchar, sep = ":")
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

# Split dashboard table into running/queued/finished views with requested sorting.
# Args:
# - tab: Dashboard table.
# Returns:
# - Named list with data.frames: running, queued, finished.
split_dashboard_tasks <- function(tab) {
  if (nrow(tab) == 0) {
    return(list(
      running = tab,
      queued = tab,
      finished = tab
    ))
  }

  running <- tab[tab$status == "running", , drop = FALSE]
  queued <- tab[tab$status == "queued", , drop = FALSE]
  finished <- tab[tab$status %in% c("completed", "failed", "cancelled"), , drop = FALSE]

  if (nrow(running) > 0) {
    running <- running[order(running$start_time, decreasing = TRUE), , drop = FALSE]
  }

  if (nrow(queued) > 0) {
    queued <- queued[order(-queued$priority, queued$submit_time), , drop = FALSE]
  }

  if (nrow(finished) > 0) {
    finished <- finished[order(finished$end_time, decreasing = TRUE), , drop = FALSE]
  }

  list(running = running, queued = queued, finished = finished)
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
  n_completed <- sum(tab$status == "completed")
  n_failed <- sum(tab$status == "failed")
  n_cancelled <- sum(tab$status == "cancelled")
  n_total <- nrow(tab)

  terminal_count <- n_completed + n_failed + n_cancelled
  completion_ratio <- if (n_total == 0) 0 else terminal_count / n_total

  # Backward compatibility: older snapshots may not have a `slots` column.
  if ("slots" %in% names(tab)) {
    running_slots <- suppressWarnings(as.integer(tab$slots[tab$status == "running"]))
    if (length(running_slots) == 0) {
      slots_used <- 0L
    } else {
      running_slots[is.na(running_slots) | running_slots < 1L] <- 1L
      slots_used <- sum(running_slots)
    }
  } else {
    slots_used <- n_running
  }

  slots <- max(1L, as.integer(max_slots %||% 1L))
  slot_ratio <- min(1, slots_used / slots)

  list(
    total = n_total,
    running = n_running,
    queued = n_queued,
    completed = n_completed,
    failed = n_failed,
    cancelled = n_cancelled,
    slots_used = slots_used,
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
    canceling = "status-failed",
    running = "status-running",
    queued = "status-queued",
    completed = "status-completed",
    failed = "status-failed",
    cancelled = "status-cancelled",
    "status-unknown"
  )
}

# Map internal status codes to user-facing labels.
# Keeps internal values stable while presenting clearer wording in UI.
status_display_label <- function(status) {
  if (identical(status, "cancelled")) {
    return("cancelled")
  }
  status
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

  logs <- tryCatch(get_task_log(task_id), error = function(e) NULL)
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
    "=== Output (latest lines) ===",
    stdout_text,
    "",
    "=== Messages & Errors (latest lines) ===",
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

# Normalize mixed console output into clean display lines.
# Handles ANSI color codes and carriage-return updates used by progress bars.
normalize_console_lines <- function(text) {
  if (!is.character(text) || length(text) != 1 || is.na(text) || !nzchar(text)) {
    return(character())
  }

  clean <- gsub("\u001b\\[[0-9;]*[A-Za-z]", "", text, perl = TRUE)
  clean <- gsub("\r\n", "\n", clean, fixed = TRUE)
  lines <- unlist(strsplit(clean, "\r|\n", perl = TRUE), use.names = FALSE)
  lines <- trimws(lines)
  lines[nzchar(lines)]
}

# Detect percent contexts that usually represent metrics, not task progress.
has_non_progress_percent_context <- function(line) {
  low <- tolower(line %||% "")
  grepl("\\b(accuracy|acc|acceptance\\s+rate|error\\s+rate|failure\\s+rate|memory|cpu|gpu|ram|utili[sz]ation|usage|auc|f1|precision|recall|rhat|ess)\\b", low, perl = TRUE)
}

# Detect words that strongly suggest progress semantics.
has_progress_context <- function(line) {
  low <- tolower(line %||% "")
  grepl("\\b(progress|step|iter(?:ation)?|epoch|batch|sample|sampling|warmup|download(?:ing)?|upload(?:ing)?|process(?:ing)?|train(?:ing)?|fit(?:ting)?|complete(?:d)?|chain)\\b", low, perl = TRUE)
}

# Parse one line with explicit fraction style progress (`x/y`, `x of y`).
parse_fraction_progress_line <- function(line) {
  if (!is.character(line) || length(line) != 1 || is.na(line) || !nzchar(trimws(line))) {
    return(NA_real_)
  }

  low <- tolower(line)
  if (grepl("\\b(errors?|failed|failures|passed|tests?)\\b", low, perl = TRUE)) {
    return(NA_real_)
  }

  context_pat <- "(?:iteration|iter|step|epoch|batch|progress|sample|sampling|draw|warmup)"
  ratio_pat <- "(\\d+)\\s*(?:/|of|out\\s+of)\\s*(\\d+)"
  m_ctx <- regexec(paste0(context_pat, "[^0-9]{0,20}", ratio_pat), line, perl = TRUE, ignore.case = TRUE)
  g_ctx <- regmatches(line, m_ctx)[[1]]
  if (length(g_ctx) > 0) {
    num <- as.numeric(g_ctx[[2]])
    den <- as.numeric(g_ctx[[3]])
    if (!is.na(num) && !is.na(den) && den > 0) {
      return(max(0, min(1, num / den)))
    }
  }

  if (!has_progress_context(line)) {
    return(NA_real_)
  }

  m_any <- regexec(ratio_pat, line, perl = TRUE, ignore.case = TRUE)
  g_any <- regmatches(line, m_any)[[1]]
  if (length(g_any) == 0) {
    return(NA_real_)
  }

  num <- as.numeric(g_any[[2]])
  den <- as.numeric(g_any[[3]])
  if (is.na(num) || is.na(den) || den <= 0) {
    return(NA_real_)
  }

  max(0, min(1, num / den))
}

# Parse one line with percent style progress (`45%`), with safety filters.
parse_percent_progress_line <- function(line) {
  if (!is.character(line) || length(line) != 1 || is.na(line) || !nzchar(trimws(line))) {
    return(NA_real_)
  }

  if (has_non_progress_percent_context(line)) {
    return(NA_real_)
  }

  m <- gregexpr("(\\d{1,3}(?:\\.\\d+)?)\\s*%", line, perl = TRUE, ignore.case = TRUE)
  vals <- regmatches(line, m)[[1]]
  if (length(vals) == 0) {
    return(NA_real_)
  }

  pct <- suppressWarnings(as.numeric(gsub("%", "", vals[length(vals)], fixed = TRUE)))
  if (is.na(pct)) {
    return(NA_real_)
  }

  max(0, min(1, pct / 100))
}

# Parse one line with bounded visual progress bars (e.g., `[###---]`, `|***   |`).
parse_visual_bar_progress_line <- function(line) {
  if (!is.character(line) || length(line) != 1 || is.na(line) || !nzchar(trimws(line))) {
    return(NA_real_)
  }

  block_chars <- intToUtf8(c(0x2588, 0x2593, 0x2592))
  bar_pats <- c(
    sprintf("\\[([#=+\\.\\- _*%s]{5,})\\]", block_chars),
    sprintf("\\|([#=+\\.\\- _*%s]{5,})\\|", block_chars)
  )
  for (pat in bar_pats) {
    m <- regexec(pat, line, perl = TRUE)
    g <- regmatches(line, m)[[1]]
    if (length(g) == 0) {
      next
    }

    bar <- g[[2]]
    width <- nchar(bar, type = "chars")
    if (is.na(width) || width < 5) {
      next
    }

    chars <- strsplit(bar, "", fixed = TRUE)[[1]]
    fill_count <- sum(chars %in% c("#", "=", "+", "*", strsplit(block_chars, "", fixed = TRUE)[[1]]))
    if (fill_count <= 0) {
      next
    }

    return(max(0, min(1, fill_count / width)))
  }

  NA_real_
}

# Parse generic task progress from logs using fixed priority:
# fraction > percent > visual bar.
parse_generic_progress_fraction <- function(text) {
  lines <- normalize_console_lines(text)
  if (length(lines) == 0) {
    return(NA_real_)
  }

  latest <- NA_real_
  for (line in lines) {
    fraction <- parse_fraction_progress_line(line)
    if (is.na(fraction)) {
      fraction <- parse_percent_progress_line(line)
    }
    if (is.na(fraction)) {
      fraction <- parse_visual_bar_progress_line(line)
    }
    if (!is.na(fraction) && is.finite(fraction)) {
      latest <- max(0, min(1, fraction))
    }
  }

  latest
}

# Parse generic chain progress rows from lines like:
# "Chain 2 step 20/100" or "Chain 2: progress 45%".
extract_chain_progress_rows <- function(text) {
  empty_tab <- function() {
    data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      stringsAsFactors = FALSE
    )
  }

  lines <- normalize_console_lines(text)
  if (length(lines) == 0) {
    return(empty_tab())
  }

  rows <- list()
  for (line in lines) {
    chain_match <- regexec("chain\\s*(\\d+)", line, ignore.case = TRUE, perl = TRUE)
    chain_groups <- regmatches(line, chain_match)[[1]]
    if (length(chain_groups) == 0) {
      next
    }
    chain_id <- as.integer(chain_groups[[2]])
    if (is.na(chain_id)) {
      next
    }

    fraction <- parse_fraction_progress_line(line)
    if (is.na(fraction)) {
      fraction <- parse_percent_progress_line(line)
    }
    if (is.na(fraction)) {
      fraction <- parse_visual_bar_progress_line(line)
    }
    if (is.na(fraction) || !is.finite(fraction)) {
      next
    }

    rows[[length(rows) + 1L]] <- data.frame(
      chain = chain_id,
      progress = max(0, min(1, fraction)),
      phase = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    return(empty_tab())
  }

  all_rows <- do.call(rbind, rows)
  latest_idx <- tapply(seq_len(nrow(all_rows)), all_rows$chain, function(idx) tail(idx, 1))
  out <- all_rows[as.integer(latest_idx), , drop = FALSE]
  out <- out[order(out$chain), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Decide display progress for Dashboard running cards.
# Priority:
# 1) newly parsed progress from logs
# 2) task-provided progress (`report_progress`)
# 3) cached display progress (freeze previous value)
# 4) default fallback (50%)
resolve_dashboard_progress_fraction <- function(
    parsed_fraction,
    task_fraction = NA_real_,
    cached_fraction = NA_real_,
    default_fraction = 0.5,
    initial_fraction = 0.01,
    start_time = NA,
    now = Sys.time(),
    fallback_after_sec = 3) {
  elapsed_sec <- suppressWarnings(as.numeric(difftime(now, start_time, units = "secs")))
  has_elapsed <- is.finite(elapsed_sec) && !is.na(elapsed_sec)
  use_fallback <- isTRUE(has_elapsed && elapsed_sec >= as.numeric(fallback_after_sec))

  if (!is.na(parsed_fraction) && is.finite(parsed_fraction)) {
    return(max(0, min(1, as.numeric(parsed_fraction))))
  }
  if (!is.na(task_fraction) && is.finite(task_fraction)) {
    return(max(0, min(1, as.numeric(task_fraction))))
  }
  if (!is.na(cached_fraction) && is.finite(cached_fraction)) {
    cached <- max(0, min(1, as.numeric(cached_fraction)))
    if (cached > as.numeric(initial_fraction)) {
      return(cached)
    }
    if (!use_fallback) {
      return(cached)
    }
    return(max(0, min(1, as.numeric(default_fraction))))
  }

  if (!use_fallback) {
    return(max(0, min(1, as.numeric(initial_fraction))))
  }

  max(0, min(1, as.numeric(default_fraction)))
}

# Resolve how long dashboard should keep the initial 1% placeholder before
# falling back to 50% when no progress is identifiable.
# Reads option `taskr.dashboard.initial_progress_wait_sec` with default `3`.
dashboard_initial_progress_wait_sec <- function() {
  wait_sec <- getOption("taskr.dashboard.initial_progress_wait_sec", 3)
  wait_sec <- suppressWarnings(as.numeric(wait_sec))
  if (length(wait_sec) != 1 || is.na(wait_sec) || !is.finite(wait_sec) || wait_sec < 0) {
    return(3)
  }
  wait_sec
}


# Parse one task's logs for Dashboard display.
# Returns chain-level progress when available; otherwise a single fraction.
dashboard_parse_task_progress <- function(task_id) {
  out <- list(
    chain = data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      stringsAsFactors = FALSE
    ),
    fraction = NA_real_
  )

  if (is.null(task_id) || length(task_id) == 0 || is.na(task_id) || !nzchar(task_id)) {
    return(out)
  }

  logs <- tryCatch(get_task_log(task_id), error = function(e) NULL)
  if (is.null(logs)) {
    return(out)
  }

  text <- paste(logs$stdout %||% "", logs$stderr %||% "", sep = "\n")

  chain_tab <- extract_stan_progress_rows(text)
  if (nrow(chain_tab) == 0) {
    chain_tab <- extract_chain_progress_rows(text)
  }

  if (nrow(chain_tab) > 0) {
    out$chain <- chain_tab[, c("chain", "progress", "phase"), drop = FALSE]
    return(out)
  }

  out$fraction <- parse_generic_progress_fraction(text)
  out
}

# Parse per-chain Stan/model progress from one task's logs.
# Args:
# - task_id: Task id used by `get_task_log()`.
# Returns:
# - data.frame with columns `chain`, `progress`, and `phase`.
dashboard_stan_chain_progress <- function(task_id) {
  dashboard_parse_task_progress(task_id)$chain
}

# Build combined stdout/stderr tail text directly from one task row.
# Args:
# - task: One-row data.frame containing `stdout` and `stderr`.
# - tail_n: Maximum number of lines kept per stream.
# Returns:
# - Multi-line text block for inline detail display.
dashboard_log_text_from_row <- function(task, tail_n = 200L) {
  if (is.null(task) || nrow(task) == 0) {
    return("Task logs are not available for the selected task.")
  }

  stdout_lines <- unlist(strsplit(task$stdout[[1]] %||% "", "\n", fixed = TRUE), use.names = FALSE)
  stderr_lines <- unlist(strsplit(task$stderr[[1]] %||% "", "\n", fixed = TRUE), use.names = FALSE)

  if (length(stdout_lines) > tail_n) {
    stdout_lines <- tail(stdout_lines, tail_n)
  }
  if (length(stderr_lines) > tail_n) {
    stderr_lines <- tail(stderr_lines, tail_n)
  }

  stdout_text <- if (length(stdout_lines) == 0) "(empty)" else paste(stdout_lines, collapse = "\n")
  stderr_text <- if (length(stderr_lines) == 0) "(empty)" else paste(stderr_lines, collapse = "\n")

  paste(
    "=== Output (latest lines) ===",
    stdout_text,
    "",
    "=== Messages & Errors (latest lines) ===",
    stderr_text,
    sep = "\n"
  )
}

# Parse per-chain progress directly from one task row.
# Args:
# - task: One-row data.frame containing `stdout` and `stderr`.
# Returns:
# - data.frame with columns `chain`, `progress`, and `phase`.
dashboard_stan_chain_progress_from_row <- function(task) {
  if (is.null(task) || nrow(task) == 0) {
    return(data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      stringsAsFactors = FALSE
    ))
  }

  text <- paste(task$stdout[[1]] %||% "", task$stderr[[1]] %||% "", sep = "\n")
  tab <- extract_stan_progress_rows(text)
  if (nrow(tab) == 0) {
    tab <- extract_chain_progress_rows(text)
  }
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

# Parse task progress directly from one task row.
# Args:
# - task: One-row data.frame containing `stdout` and `stderr`.
# Returns:
# - List with `chain` data.frame and scalar `fraction`.
dashboard_parse_task_progress_from_row <- function(task) {
  out <- list(
    chain = data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      stringsAsFactors = FALSE
    ),
    fraction = NA_real_
  )

  if (is.null(task) || nrow(task) == 0) {
    return(out)
  }

  text <- paste(task$stdout[[1]] %||% "", task$stderr[[1]] %||% "", sep = "\n")
  chain_tab <- extract_stan_progress_rows(text)
  if (nrow(chain_tab) == 0) {
    chain_tab <- extract_chain_progress_rows(text)
  }

  if (nrow(chain_tab) > 0) {
    out$chain <- chain_tab[, c("chain", "progress", "phase"), drop = FALSE]
    return(out)
  }

  out$fraction <- parse_generic_progress_fraction(text)
  out
}
