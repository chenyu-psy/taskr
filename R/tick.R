# Queue Tick Core (Internal)
#
# Purpose:
# - Advance queue state by one scheduler step:
#   1) recycle running tasks
#   2) sort pending tasks by priority and submit order
#   3) start launchable tasks within slot capacity
#
# Notes:
# - This file is queue-layer logic only.
# - It must not call `callr::*` directly. It only talks to task objects through
#   the contract defined in `R/contract.R`.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

task_slots <- function(task_item) {
  as.integer(task_item$resources$slots %||% 1L)
}

running_slots_used <- function(running) {
  if (length(running) == 0) {
    return(0L)
  }

  sum(vapply(running, task_slots, integer(1)))
}

sort_pending_by_priority <- function(pending) {
  if (length(pending) <= 1) {
    return(pending)
  }

  priority <- vapply(pending, function(x) as.integer(x$priority %||% 0L), integer(1))
  submit_time <- vapply(
    pending,
    function(x) as.numeric(x$submit_time %||% 0),
    numeric(1)
  )
  order_idx <- order(-priority, submit_time)
  pending[order_idx]
}

# Convert one pending/running item into a cancelled finished record.
# Args:
# - item: Scheduler item from pending or running buckets.
# - now: Timestamp used as the cancellation end time.
# Returns:
# - The same task item with terminal cancelled fields set.
cancel_item_record <- function(item, now) {
  item$status <- "cancelled"
  item$end_time <- now
  item$error <- item$error %||% "Task canceled by user."
  item
}

# Apply dashboard cancel markers before launching or recycling tasks.
# Args:
# - state: Scheduler state with pending/running/finished buckets.
# - now: Timestamp used for cancellation records.
# Returns:
# - Updated scheduler state.
# Side effects:
# - Kills running task objects when possible and removes consumed marker files.
apply_dashboard_cancel_requests <- function(state, now) {
  if (is.null(state)) {
    return(state)
  }

  if (length(state$pending) > 0) {
    remaining_pending <- list()
    for (item in state$pending) {
      task_id <- as.character(item$id %||% "")
      if (nzchar(task_id) && dashboard_cancel_requested(task_id)) {
        state$finished[[task_id]] <- cancel_item_record(item, now = now)
        clear_dashboard_cancel_marker(task_id)
      } else {
        remaining_pending[[length(remaining_pending) + 1L]] <- item
      }
    }
    state$pending <- remaining_pending
  }

  if (length(state$running) > 0) {
    running_ids <- names(state$running)
    for (task_id in running_ids) {
      if (!dashboard_cancel_requested(task_id)) {
        next
      }

      item <- state$running[[task_id]]
      task <- item$task %||% NULL
      kill_error <- tryCatch({
        if (!is.null(task)) {
          task$kill()
        }
        NULL
      }, error = function(e) conditionMessage(e))

      if (!is.null(kill_error)) {
        item$error <- kill_error
        state$running[[task_id]] <- item
        next
      }

      state$running[[task_id]] <- NULL
      state$finished[[task_id]] <- cancel_item_record(item, now = now)
      clear_dashboard_cancel_marker(task_id)
    }
  }

  state
}

# Apply a dashboard request to remove all finished task records.
# Args:
# - state: Scheduler state with pending/running/finished buckets.
# Returns:
# - Updated scheduler state.
# Side effects:
# - Deletes result files for finished records and removes the cleanup marker.
apply_dashboard_clean_finished_request <- function(state) {
  if (is.null(state) || !dashboard_clean_finished_requested()) {
    return(state)
  }

  finished_items <- state$finished %||% list()
  if (length(finished_items) > 0) {
    for (item in finished_items) {
      delete_task_result_file(item)
    }
  }

  state$finished <- list()
  clear_dashboard_clean_finished_marker()
  state
}

recycle_running_tasks <- function(state, now) {
  if (length(state$running) == 0) {
    return(state)
  }

  ids <- names(state$running)
  keep_running <- list()

  for (id in ids) {
    item <- state$running[[id]]
    task <- item$task

    if (is.null(task)) {
      keep_running[[id]] <- item
      next
    }

    stdout_chunk <- tryCatch(task$read_output(), error = function(e) "")
    stderr_chunk <- tryCatch(task$read_error(), error = function(e) "")
    prog <- tryCatch(task$progress(), error = function(e) NULL)
    status <- tryCatch(task$status(), error = function(e) "failed")

    item$stdout_buffer <- paste0(item$stdout_buffer %||% "", stdout_chunk)
    item$stderr_buffer <- paste0(item$stderr_buffer %||% "", stderr_chunk)

    if (!is.null(prog)) {
      item$progress <- prog$fraction %||% item$progress
      item$message <- prog$message %||% item$message
    }

    # Keep scheduler item buffers in sync with task object buffers.
    # This is important when `task$status()` internally consumes process
    # events via `process$read()` and appends to the task-level buffers.
    task_stdout <- task$stdout_buffer %||% NULL
    task_stderr <- task$stderr_buffer %||% NULL
    if (!is.null(task_stdout)) {
      item$stdout_buffer <- task_stdout
    }
    if (!is.null(task_stderr)) {
      item$stderr_buffer <- task_stderr
    }

    latest_prog <- tryCatch(task$progress(), error = function(e) NULL)
    if (!is.null(latest_prog)) {
      item$progress <- latest_prog$fraction %||% item$progress
      item$message <- latest_prog$message %||% item$message
    }

    if (identical(status, "running")) {
      keep_running[[id]] <- item
      next
    }

    item$status <- status
    item$end_time <- now
    if (!is.null(task$error)) {
      item$error <- task$error
    }
    proc <- task$process %||% NULL
    if (!is.null(proc) && is.function(proc$close)) {
      try(proc$close(), silent = TRUE)
    }
    state$finished[[id]] <- item
  }

  state$running <- keep_running
  state
}

launch_from_queue <- function(state, now, start_task_fn = NULL) {
  if (length(state$pending) == 0) {
    return(state)
  }

  state$pending <- sort_pending_by_priority(state$pending)
  capacity_slots <- as.integer(state$capacity$slots %||% 1L)
  available_slots <- capacity_slots - running_slots_used(state$running)
  remaining_pending <- list()

  for (item in state$pending) {
    item_slots <- task_slots(item)

    if (item_slots <= available_slots) {
      launcher <- start_task_fn %||% item$start_task
      if (is.null(launcher) || !is.function(launcher)) {
        stop("Pending task must provide `start_task` or `update_queue()` must receive `start_task_fn`.")
      }

      task_obj <- launcher(item)
      item$task <- task_obj
      item$status <- "running"
      item$start_time <- now
      state$running[[task_id_key(item$id)]] <- item
      available_slots <- available_slots - item_slots
    } else {
      remaining_pending[[length(remaining_pending) + 1L]] <- item
    }
  }

  state$pending <- remaining_pending
  state
}

update_queue <- function(state, start_task_fn = NULL, now = Sys.time()) {
  state <- apply_dashboard_cancel_requests(state, now = now)
  state <- recycle_running_tasks(state, now = now)
  state <- apply_dashboard_cancel_requests(state, now = now)
  state <- apply_dashboard_clean_finished_request(state)
  state <- launch_from_queue(state, now = now, start_task_fn = start_task_fn)
  state$scheduler_should_stop <- length(state$running) == 0 && length(state$pending) == 0
  state
}
