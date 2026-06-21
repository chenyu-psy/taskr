# Task Id Helpers (Internal)
#
# Purpose:
# - Keep task ids numeric in task records and public APIs.
# - Convert ids to character only at storage boundaries such as file names,
#   environments, named lists, JSON, and DOM attributes.

normalize_task_ids <- function(id, allow_multiple = FALSE, name = "id") {
  if (is.null(id) || length(id) == 0) {
    stop("`", name, "` must be a positive integer task id.")
  }
  if (!isTRUE(allow_multiple) && length(id) != 1) {
    stop("`", name, "` must be a single positive integer task id.")
  }

  if (is.numeric(id) && !is.logical(id)) {
    if (any(is.na(id)) || any(!is.finite(id)) || any(id <= 0) || any(id != floor(id))) {
      stop("`", name, "` must contain positive integer task ids.")
    }
    return(as.integer(id))
  }

  if (is.character(id)) {
    if (any(is.na(id)) || any(!nzchar(id)) || any(!grepl("^[0-9]+$", id))) {
      stop("`", name, "` must contain positive integer task ids.")
    }
    out <- suppressWarnings(as.integer(id))
    if (any(is.na(out)) || any(out <= 0L)) {
      stop("`", name, "` must contain positive integer task ids.")
    }
    return(out)
  }

  stop("`", name, "` must contain positive integer task ids.")
}

normalize_task_id <- function(id, name = "id") {
  normalize_task_ids(id, allow_multiple = FALSE, name = name)
}

task_id_key <- function(id) {
  as.character(normalize_task_id(id))
}
