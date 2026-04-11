# Dashboard Snapshot Storage (Internal)
#
# Purpose:
# - Persist lightweight queue state snapshots to a JSON file.
# - Allow a read-only dashboard process to monitor queue state across processes
#   within the same R session.

dashboard_snapshot_path <- function() {
  path <- pkg_env$dashboard_snapshot_path %||% NULL
  if (is.null(path) || !is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("Dashboard snapshot path is not initialized.")
  }
  path
}

write_dashboard_snapshot <- function(now = Sys.time()) {
  path <- dashboard_snapshot_path()
  state <- pkg_env$scheduler %||% NULL
  tab <- dashboard_snapshot_table_from_state(state)
  tab <- add_dashboard_derived_columns(tab, now = now)

  payload <- list(
    generated_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    max_concurrent = as.integer(state$capacity$slots %||% 1L),
    tasks = tab
  )

  json_txt <- jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows", null = "null")
  writeLines(json_txt, con = path, useBytes = TRUE)
  invisible(path)
}

read_dashboard_snapshot <- function(path = dashboard_snapshot_path()) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(list(
      generated_at = NA_character_,
      max_concurrent = 1L,
      tasks = empty_dashboard_table()
    ))
  }

  payload <- tryCatch(
    jsonlite::fromJSON(path, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )

  if (is.null(payload)) {
    return(list(
      generated_at = NA_character_,
      max_concurrent = 1L,
      tasks = empty_dashboard_table()
    ))
  }

  tab <- payload$tasks %||% empty_dashboard_table()
  if (!is.data.frame(tab)) {
    tab <- empty_dashboard_table()
  }

  needed <- names(empty_dashboard_table())
  for (nm in setdiff(needed, names(tab))) {
    tab[[nm]] <- empty_dashboard_table()[[nm]]
  }
  tab <- tab[, needed, drop = FALSE]

  if (nrow(tab) > 0) {
    tab$submit_time <- as.POSIXct(tab$submit_time, origin = "1970-01-01")
    tab$start_time <- as.POSIXct(tab$start_time, origin = "1970-01-01")
    tab$end_time <- as.POSIXct(tab$end_time, origin = "1970-01-01")
  }

  list(
    generated_at = payload$generated_at %||% NA_character_,
    max_concurrent = as.integer(payload$max_concurrent %||% 1L),
    tasks = tab
  )
}
