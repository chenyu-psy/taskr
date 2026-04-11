# Task Listing API (User-Facing)
#
# Purpose:
# - Provide one query entrypoint for task records in queue/running/done states.
# - Return a stable data.frame shape so downstream code can rely on columns.

empty_task_table <- function() {
  data.frame(
    id = character(),
    label = character(),
    status = character(),
    progress = numeric(),
    message = character(),
    elapsed = character(),
    error = character(),
    submit_time = character(),
    start_time = character(),
    end_time = character(),
    stringsAsFactors = FALSE
  )
}

format_task_time <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_character_)
  }

  format(as.POSIXct(x, origin = "1970-01-01"), "%m-%d %H:%M:%S")
}

format_elapsed <- function(seconds) {
  if (is.null(seconds) || length(seconds) == 0 || is.na(seconds) || !is.finite(seconds)) {
    return(NA_character_)
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

task_item_elapsed <- function(item, now = Sys.time()) {
  start_time <- item$start_time %||% NA
  end_time <- item$end_time %||% NA

  if (is.null(start_time) || length(start_time) == 0 || is.na(start_time)) {
    return(NA_real_)
  }

  if (is.null(end_time) || length(end_time) == 0 || is.na(end_time)) {
    end_time <- now
  }

  as.numeric(difftime(end_time, start_time, units = "secs"))
}

task_item_to_row <- function(item, now = Sys.time()) {
  data.frame(
    id = as.character(item$id %||% NA_character_),
    label = as.character(item$label %||% NA_character_),
    status = as.character(item$status %||% NA_character_),
    progress = as.numeric(item$progress %||% NA_real_),
    message = as.character(item$message %||% NA_character_),
    elapsed = format_elapsed(task_item_elapsed(item = item, now = now)),
    error = as.character(item$error %||% NA_character_),
    submit_time = format_task_time(item$submit_time %||% NA),
    start_time = format_task_time(item$start_time %||% NA),
    end_time = format_task_time(item$end_time %||% NA),
    stringsAsFactors = FALSE
  )
}

collect_scheduler_items <- function(state) {
  c(state$queue %||% list(), unname(state$running %||% list()), unname(state$done %||% list()))
}

sort_task_table <- function(tab) {
  if (nrow(tab) == 0) {
    return(tab)
  }

  status_levels <- c("done", "running", "queued", "failed", "cancelled")
  status_rank <- match(tab$status, status_levels)
  status_rank[is.na(status_rank)] <- length(status_levels) + 1L

  tab[order(status_rank, tab$id), , drop = FALSE]
}

validate_task_filter <- function(x, name) {
  if (is.null(x)) {
    return(invisible(NULL))
  }

  if (!is.character(x) || any(is.na(x))) {
    stop("`", name, "` must be NULL or a character vector without missing values.")
  }

  invisible(NULL)
}

#' List Tasks in the Current Scheduler
#'
#' Purpose:
#' - Return task records from queue, running, and done states in one table.
#' - Optionally filter by `id`, `label`, and/or `status` using AND logic.
#'
#' @param id Optional character vector of task ids to include.
#' @param label Optional character vector of labels to include.
#' @param status Optional character vector of statuses to include.
#' @return A `data.frame` with one row per matched task.
#' @examples
#' init_queue(max_concurrent = 1)
#' list_tasks()
#' @export
list_tasks <- function(id = NULL, label = NULL, status = NULL) {
  validate_task_filter(id, "id")
  validate_task_filter(label, "label")
  validate_task_filter(status, "status")

  if (is.null(pkg_env$scheduler)) {
    return(empty_task_table())
  }

  pkg_env$scheduler <- recycle_running_tasks(pkg_env$scheduler, now = Sys.time())
  if (scheduler_has_work(pkg_env$scheduler)) {
    start_scheduler_internal()
  }

  items <- collect_scheduler_items(pkg_env$scheduler)
  if (length(items) == 0) {
    return(empty_task_table())
  }

  now <- Sys.time()
  rows <- lapply(items, task_item_to_row, now = now)
  out <- do.call(rbind, rows)

  if (!is.null(id)) {
    out <- out[out$id %in% id, , drop = FALSE]
  }
  if (!is.null(label)) {
    out <- out[out$label %in% label, , drop = FALSE]
  }
  if (!is.null(status)) {
    out <- out[out$status %in% status, , drop = FALSE]
  }

  rownames(out) <- NULL
  if (nrow(out) == 0) {
    return(empty_task_table())
  }

  out <- sort_task_table(out)
  rownames(out) <- NULL
  out
}
