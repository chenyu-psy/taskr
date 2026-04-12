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
    max_slots = as.integer(state$capacity$slots %||% 1L),
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
      max_slots = 1L,
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
      max_slots = 1L,
      tasks = empty_dashboard_table()
    ))
  }

  tab <- payload$tasks %||% empty_dashboard_table()
  if (!is.data.frame(tab)) {
    tab <- empty_dashboard_table()
  }

  needed <- names(empty_dashboard_table())
  n <- nrow(tab)
  for (nm in setdiff(needed, names(tab))) {
    template <- empty_dashboard_table()[[nm]]
    if (inherits(template, "POSIXct")) {
      tab[[nm]] <- rep(as.POSIXct(NA), n)
    } else if (is.integer(template)) {
      tab[[nm]] <- rep(NA_integer_, n)
    } else if (is.numeric(template)) {
      tab[[nm]] <- rep(NA_real_, n)
    } else {
      tab[[nm]] <- rep(NA_character_, n)
    }
  }
  tab <- tab[, needed, drop = FALSE]

  as_posix_snapshot <- function(x, n) {
    if (is.list(x)) {
      x <- vapply(
        x,
        function(el) {
          if (is.null(el) || length(el) == 0 || all(is.na(el))) {
            return(NA_character_)
          }
          as.character(el[[1]])
        },
        character(1)
      )
    }

    if (inherits(x, "POSIXct")) {
      return(as.POSIXct(x))
    }
    if (is.numeric(x)) {
      return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
    }

    # Snapshot JSON stores POSIXct as plain wall-clock strings without timezone.
    # Parse back in local timezone so elapsed/start labels remain consistent
    # between writer and reader processes on the same machine.
    out <- suppressWarnings(as.POSIXct(as.character(x), tz = ""))
    if (length(out) == 0) {
      out <- rep(as.POSIXct(NA), n)
    }
    out
  }

  tab$submit_time <- as_posix_snapshot(tab$submit_time, n = n)
  tab$start_time <- as_posix_snapshot(tab$start_time, n = n)
  tab$end_time <- as_posix_snapshot(tab$end_time, n = n)

  list(
    generated_at = payload$generated_at %||% NA_character_,
    max_slots = as.integer(payload$max_slots %||% 1L),
    tasks = tab
  )
}
