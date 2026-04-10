# Task Lookup Helpers (Internal)
#
# Purpose:
# - Centralize id/label lookup and label index maintenance.

all_task_items_with_location <- function(state) {
  out <- list()

  for (i in seq_along(state$queue %||% list())) {
    item <- state$queue[[i]]
    out[[length(out) + 1L]] <- list(bucket = "queue", index = i, item = item)
  }

  running_ids <- names(state$running %||% list())
  for (rid in running_ids) {
    item <- state$running[[rid]]
    out[[length(out) + 1L]] <- list(bucket = "running", index = rid, item = item)
  }

  done_ids <- names(state$done %||% list())
  for (did in done_ids) {
    item <- state$done[[did]]
    out[[length(out) + 1L]] <- list(bucket = "done", index = did, item = item)
  }

  out
}

validate_unique_label <- function(state, label) {
  if (is.null(label)) {
    return(invisible(NULL))
  }

  existing <- state$label_index[[label]] %||% NULL
  if (!is.null(existing)) {
    stop("Task label `", label, "` already exists. Labels must be unique.")
  }

  invisible(NULL)
}

remove_label_index_entry <- function(state, item) {
  label <- item$label %||% NULL
  if (is.null(label)) {
    return(state)
  }

  mapped <- state$label_index[[label]] %||% NULL
  if (!is.null(mapped) && identical(mapped, item$id)) {
    state$label_index[[label]] <- NULL
  }

  state
}

resolve_task_reference <- function(state, id_or_label) {
  validate_id_or_label(id_or_label)
  located <- all_task_items_with_location(state)
  if (length(located) == 0) {
    return(NULL)
  }

  by_id <- located[vapply(located, function(x) identical(x$item$id %||% NA_character_, id_or_label), logical(1))]
  if (length(by_id) == 1) {
    return(by_id[[1]])
  }
  if (length(by_id) > 1) {
    stop("More than one task matches id `", id_or_label, "`.")
  }

  indexed_id <- state$label_index[[id_or_label]] %||% NULL
  if (!is.null(indexed_id)) {
    by_index <- located[vapply(located, function(x) identical(x$item$id %||% NA_character_, indexed_id), logical(1))]
    if (length(by_index) == 1) {
      return(by_index[[1]])
    }
  }

  by_label <- located[vapply(located, function(x) identical(x$item$label %||% NA_character_, id_or_label), logical(1))]
  if (length(by_label) == 0) {
    return(NULL)
  }
  if (length(by_label) > 1) {
    stop(
      "More than one task matches `id_or_label = ", id_or_label, "`. ",
      "Use a unique id or label."
    )
  }

  by_label[[1]]
}
