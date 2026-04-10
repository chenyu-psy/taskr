# Queue Overview API (User-Facing)
#
# Purpose:
# - Print a compact overview of queue usage and task status counts.

queue_status_counts <- function(tab) {
  statuses <- c("queued", "running", "done", "failed", "killed")
  counts <- stats::setNames(integer(length(statuses)), statuses)
  if (nrow(tab) == 0) {
    return(counts)
  }

  observed <- table(tab$status)
  for (name in names(observed)) {
    if (name %in% names(counts)) {
      counts[[name]] <- as.integer(observed[[name]])
    }
  }

  counts
}

print_queue_overview_lines <- function(lines) {
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_h1(lines[[1]])
    for (line in lines[-1]) {
      cli::cli_text("{line}")
    }
    return(invisible(NULL))
  }

  for (line in lines) {
    cat(line, "\n", sep = "")
  }

  invisible(NULL)
}

#' Print a Queue Overview
#'
#' Purpose:
#' - Show current queue capacity usage and task counts by status.
#'
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_concurrent = 2)
#' queue_overview()
#' @export
queue_overview <- function() {
  if (is.null(pkg_env$scheduler)) {
    print_queue_overview_lines(c("taskr queue", "Queue is not initialized."))
    return(invisible(NULL))
  }

  tab <- list_tasks()
  counts <- queue_status_counts(tab)

  capacity_slots <- as.integer(pkg_env$scheduler$capacity$slots %||% 1L)
  used_slots <- running_slots_used(pkg_env$scheduler$running %||% list())
  terminal_total <- counts[["done"]] + counts[["failed"]] + counts[["killed"]]

  lines <- c(
    "taskr queue",
    paste0("Capacity: ", used_slots, "/", capacity_slots, " slots used"),
    paste0("Queued: ", counts[["queued"]]),
    paste0("Running: ", counts[["running"]]),
    paste0("Terminal: ", terminal_total, " (done=", counts[["done"]],
           ", failed=", counts[["failed"]], ", killed=", counts[["killed"]], ")")
  )

  print_queue_overview_lines(lines)
  invisible(NULL)
}
