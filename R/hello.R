#' Return a Friendly Package Message
#'
#' Purpose: provide a minimal exported function so the package has
#' a clear, testable starting point.
#'
#' Parameters:
#' - `name` (`character(1)`): Name to include in the greeting.
#'
#' Returns:
#' - `character(1)`: A greeting string.
#'
#' Assumptions and side effects:
#' - Assumes `name` is a single non-missing character value.
#' - No side effects; this function is pure.
#'
#' @param name Character string used in the greeting.
#' @return A single greeting string.
#' @examples
#' hello_taskr("researcher")
#' @export
hello_taskr <- function(name = "researcher") {
  if (!is.character(name) || length(name) != 1 || is.na(name)) {
    stop("`name` must be a single non-missing character string.")
  }

  paste0("Hello, ", name, "! Welcome to taskr.")
}
