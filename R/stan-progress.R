# Stan Progress Helpers (User-Facing)
#
# Purpose:
# - Parse Stan chain progress from task logs.
# - Print a compact per-chain progress bar summary.

extract_stan_progress_rows <- function(text) {
  if (!is.character(text) || length(text) != 1 || is.na(text) || !nzchar(text)) {
    return(data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      line = character(),
      stringsAsFactors = FALSE
    ))
  }

  lines <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    return(data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      line = character(),
      stringsAsFactors = FALSE
    ))
  }

  # CmdStan-style line example:
  # "Chain 1 Iteration: 500 / 1000 [ 50%] (Warmup)"
  cmdstan_pat <- "^\\s*Chain\\s+(\\d+)\\s+Iteration:\\s*(\\d+)\\s*/\\s*(\\d+)\\s*\\[\\s*(\\d+)%\\]"
  # RStan-style fallback:
  # "Chain 2: 60% warmup"
  rstan_pat <- "^\\s*Chain\\s+(\\d+).*?(\\d+)%"

  rows <- list()
  for (line in lines) {
    m1 <- regexec(cmdstan_pat, line, perl = TRUE)
    g1 <- regmatches(line, m1)[[1]]

    if (length(g1) > 0) {
      chain <- as.integer(g1[[2]])
      pct <- as.numeric(g1[[5]])
      phase <- NA_character_
      if (grepl("Warmup", line, fixed = TRUE)) phase <- "Warmup"
      if (grepl("Sampling", line, fixed = TRUE)) phase <- "Sampling"

      rows[[length(rows) + 1L]] <- data.frame(
        chain = chain,
        progress = max(0, min(1, pct / 100)),
        phase = phase,
        line = line,
        stringsAsFactors = FALSE
      )
      next
    }

    m2 <- regexec(rstan_pat, line, perl = TRUE)
    g2 <- regmatches(line, m2)[[1]]
    if (length(g2) > 0) {
      chain <- as.integer(g2[[2]])
      pct <- as.numeric(g2[[3]])
      phase <- NA_character_
      if (grepl("warmup", line, ignore.case = TRUE)) phase <- "Warmup"
      if (grepl("sampling", line, ignore.case = TRUE)) phase <- "Sampling"

      rows[[length(rows) + 1L]] <- data.frame(
        chain = chain,
        progress = max(0, min(1, pct / 100)),
        phase = phase,
        line = line,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      line = character(),
      stringsAsFactors = FALSE
    ))
  }

  all_rows <- do.call(rbind, rows)
  latest_idx <- tapply(seq_len(nrow(all_rows)), all_rows$chain, function(idx) tail(idx, 1))
  out <- all_rows[as.integer(latest_idx), , drop = FALSE]
  out <- out[order(out$chain), , drop = FALSE]
  rownames(out) <- NULL
  out
}

progress_bar_text <- function(progress, width = 24L) {
  width <- as.integer(width)
  if (is.na(width) || width < 5L) {
    width <- 24L
  }

  p <- max(0, min(1, as.numeric(progress)))
  filled <- as.integer(round(p * width))
  empty <- width - filled
  paste0("[", paste0(rep("#", filled), collapse = ""), paste0(rep("-", empty), collapse = ""), "]")
}

#' Print Stan Chain Progress Bars from Task Logs
#'
#' Purpose:
#' - Read one task's logs and summarize per-chain Stan progress in a compact
#'   progress-bar format.
#'
#' @param id_or_label Task id or label.
#' @param width Width of each text progress bar.
#' @return Invisibly returns a data.frame with one row per chain and columns
#'   `chain`, `progress`, and `phase`.
#' @examples
#' init_queue(max_concurrent = 1)
#' # stan_progress("fit_model")
#' @export
stan_progress <- function(id_or_label, width = 24L) {
  logs <- task_logs(id_or_label)
  text <- paste(logs$stdout %||% "", logs$stderr %||% "", sep = "\n")
  tab <- extract_stan_progress_rows(text)

  if (nrow(tab) == 0) {
    cat("No Stan chain progress found in logs.\n")
    return(invisible(tab))
  }

  for (i in seq_len(nrow(tab))) {
    chain <- tab$chain[[i]]
    p <- tab$progress[[i]]
    pct <- as.integer(round(p * 100))
    bar <- progress_bar_text(p, width = width)
    phase <- tab$phase[[i]]

    if (!is.na(phase) && nzchar(phase)) {
      cat(sprintf("Chain %d %s %3d%% (%s)\n", chain, bar, pct, phase))
    } else {
      cat(sprintf("Chain %d %s %3d%%\n", chain, bar, pct))
    }
  }

  invisible(tab[, c("chain", "progress", "phase"), drop = FALSE])
}
