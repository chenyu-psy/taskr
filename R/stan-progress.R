# Stan Progress Helpers (User-Facing)
#
# Purpose:
# - Parse Stan chain progress from task logs.
# - Print a compact per-chain progress bar summary.

extract_stan_progress_rows <- function(text) {
  empty_tab <- function() {
    data.frame(
      chain = integer(),
      progress = numeric(),
      phase = character(),
      line = character(),
      stringsAsFactors = FALSE
    )
  }

  detect_phase <- function(line, phase_hint = NA_character_) {
    if (!is.na(phase_hint) && nzchar(trimws(phase_hint))) {
      return(trimws(phase_hint))
    }

    if (grepl("warm[- ]?up", line, ignore.case = TRUE)) {
      return("Warmup")
    }
    if (grepl("sampling", line, ignore.case = TRUE)) {
      return("Sampling")
    }
    if (grepl("adapt", line, ignore.case = TRUE)) {
      return("Adaptation")
    }

    NA_character_
  }

  capture_group <- function(groups, index) {
    if (length(groups) >= index) {
      return(groups[[index]])
    }
    NA_character_
  }

  if (!is.character(text) || length(text) != 1 || is.na(text) || !nzchar(text)) {
    return(empty_tab())
  }

  # Real subprocess logs may contain:
  # - carriage-return refresh (`\r`) instead of newlines,
  # - ANSI escape sequences,
  # - mixed prefixes from different runners.
  # Normalize first so parser logic depends on the runtime stream shape,
  # not on whether user code used cat/print/message.
  text <- gsub("\u001b\\[[0-9;]*[A-Za-z]", "", text, perl = TRUE)
  text <- gsub("\r", "\n", text, fixed = TRUE)

  lines <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    return(empty_tab())
  }

  # CmdStan/CmdStanR/RStan iteration line with chain id.
  # Examples:
  # "Chain 1 Iteration: 500 / 1000 [ 50%] (Warmup)"
  # "Chain 1: Iteration: 500 / 1000 [ 50%] (Warmup)"
  # Also supports prefixed/wrapped lines, such as:
  # "[1] \"Chain 1 Iteration: ...\""
  # "INFO: Chain 1 Iteration: ..."
  chain_iter_pat <- "Chain\\s*(\\d+)\\s*:?[[:space:]]*Iteration:\\s*(\\d+)\\s*/\\s*(\\d+)\\s*\\[\\s*(\\d+)%\\]\\s*(?:\\(([^\\)]*)\\))?"
  # CmdStan single-chain iteration line (no chain prefix).
  # Example:
  # "Iteration: 100 / 2000 [  5%] (Warmup)"
  iter_only_pat <- "Iteration:\\s*(\\d+)\\s*/\\s*(\\d+)\\s*\\[\\s*(\\d+)%\\]\\s*(?:\\(([^\\)]*)\\))?"
  # RStan/CmdStan fallback lines with explicit chain + percent.
  # Example:
  # "Chain 2: 75% sampling"
  chain_pct_pat <- "Chain\\s*(\\d+)\\s*:?.*?(\\d+)%"
  # JAGS text progress bar fallback.
  # Example:
  # "|++++++++++++++++++++| 40%"
  jags_text_bar_pat <- "\\|[^|]*\\|\\s*(\\d+)%"

  rows <- list()
  for (line in lines) {
    m1 <- regexec(chain_iter_pat, line, perl = TRUE)
    g1 <- regmatches(line, m1)[[1]]

    if (length(g1) > 0) {
      chain <- as.integer(g1[[2]])
      pct <- as.numeric(g1[[5]])
      phase <- detect_phase(line, phase_hint = capture_group(g1, 6))

      rows[[length(rows) + 1L]] <- data.frame(
        chain = chain,
        progress = max(0, min(1, pct / 100)),
        phase = phase,
        line = line,
        stringsAsFactors = FALSE
      )
      next
    }

    m2 <- regexec(iter_only_pat, line, perl = TRUE)
    g2 <- regmatches(line, m2)[[1]]
    if (length(g2) > 0) {
      chain <- 1L
      pct <- as.numeric(g2[[4]])
      phase <- detect_phase(line, phase_hint = capture_group(g2, 5))

      rows[[length(rows) + 1L]] <- data.frame(
        chain = chain,
        progress = max(0, min(1, pct / 100)),
        phase = phase,
        line = line,
        stringsAsFactors = FALSE
      )
      next
    }

    m3 <- regexec(chain_pct_pat, line, perl = TRUE)
    g3 <- regmatches(line, m3)[[1]]
    if (length(g3) > 0) {
      chain <- as.integer(g3[[2]])
      pct <- as.numeric(g3[[3]])
      phase <- detect_phase(line, phase_hint = NA_character_)

      rows[[length(rows) + 1L]] <- data.frame(
        chain = chain,
        progress = max(0, min(1, pct / 100)),
        phase = phase,
        line = line,
        stringsAsFactors = FALSE
      )
      next
    }

    m4 <- regexec(jags_text_bar_pat, line, perl = TRUE)
    g4 <- regmatches(line, m4)[[1]]
    if (length(g4) > 0) {
      pct <- as.numeric(g4[[2]])

      rows[[length(rows) + 1L]] <- data.frame(
        chain = 1L,
        progress = max(0, min(1, pct / 100)),
        phase = "Sampling",
        line = line,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(empty_tab())
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
