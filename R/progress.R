#' Report Task Progress from a Child Process
#'
#' Purpose:
#' - Emit a structured progress message from a background task so the parent
#'   session can read and parse it later.
#'
#' Parameters:
#' - `fraction`: Numeric progress value. Values outside `[0, 1]` are clamped.
#' - `message`: Optional single progress message string.
#'
#' Returns:
#' - Invisibly returns `NULL`.
#'
#' Assumptions and side effects:
#' - Writes one tagged JSON line to standard output.
#' - Intended to be called inside a child process started by `taskr`.
#'
#' @param fraction Numeric progress fraction for the current task.
#' @param message Optional short progress message.
#' @return Invisibly returns `NULL`.
#' @examples
#' taskr::report_progress(0.5, "halfway")
#' @export
report_progress <- function(fraction, message = NULL) {
  if (!is.numeric(fraction) || length(fraction) != 1 || is.na(fraction) || !is.finite(fraction)) {
    stop("`fraction` must be a single finite numeric value.")
  }

  if (!is.null(message)) {
    if (!is.character(message) || length(message) != 1 || is.na(message)) {
      stop("`message` must be NULL or a single non-missing character string.")
    }
  }

  fraction <- max(0, min(1, as.numeric(fraction)))
  payload <- list(
    fraction = fraction,
    message = message,
    ts = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  )

  cat(
    "##TASKR_PROGRESS##",
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
    "##/TASKR_PROGRESS##\n",
    sep = ""
  )

  invisible(NULL)
}
