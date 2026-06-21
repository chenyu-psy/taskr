# Task Lookup Helpers (Internal)
#
# Purpose:
# - Centralize task id lookup across pending/running/finished scheduler buckets.

all_task_items_with_location <- function(state) {
  out <- list()

  for (i in seq_along(state$pending %||% list())) {
    item <- state$pending[[i]]
    out[[length(out) + 1L]] <- list(bucket = "pending", index = i, item = item)
  }

  running_ids <- names(state$running %||% list())
  for (rid in running_ids) {
    item <- state$running[[rid]]
    out[[length(out) + 1L]] <- list(bucket = "running", index = rid, item = item)
  }

  finished_ids <- names(state$finished %||% list())
  for (fid in finished_ids) {
    item <- state$finished[[fid]]
    out[[length(out) + 1L]] <- list(bucket = "finished", index = fid, item = item)
  }

  out
}

resolve_task_reference <- function(state, id) {
  id <- normalize_task_id(id)
  located <- all_task_items_with_location(state)
  if (length(located) == 0) {
    return(NULL)
  }

  by_id <- located[vapply(located, function(x) identical(normalize_task_id(x$item$id %||% NA_integer_), id), logical(1))]
  if (length(by_id) == 1) {
    return(by_id[[1]])
  }
  if (length(by_id) > 1) {
    stop("More than one task matches id `", id, "`.")
  }

  NULL
}
