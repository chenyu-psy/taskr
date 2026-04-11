# Shiny Dashboard Demo Script
#
# Purpose:
# - Submit a small batch of background tasks with progress updates.
# - Open `queue_dashboard()` so users can visually verify queue behavior.
#
# How to run:
# - In an interactive R session:
#   source("inst/examples/shiny-loop-demo.R")

if (!interactive()) {
  stop("Run this script in an interactive R session.")
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Please install `pkgload` first: install.packages('pkgload')")
}

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Please install `shiny` first: install.packages('shiny')")
}

pkgload::load_all(export_all = FALSE, quiet = TRUE)

set.seed(123)

taskr::shutdown_queue()
taskr::init_queue(max_concurrent = 3)
force_all_fake_model <- TRUE

# Submit 10 tasks. Each task sleeps for a random 10-20 seconds.
for (i in 1:10) {
  idx <- i
  wait_sec <- as.integer(sample(10:20, size = 1))
  is_fake_model <- isTRUE(force_all_fake_model) || idx %% 2 == 0L

  taskr::submit_task(
    expr = {
      if (isTRUE(is_fake_model)) {
        # Fake Stan printer mode:
        # Emit per-chain progress lines so dashboard can show chain bars.
        chains <- 4L
        for (s in seq_len(wait_sec)) {
          Sys.sleep(1)
          chain_id <- ((s - 1L) %% chains) + 1L
          iter <- as.integer(round(s / wait_sec * 1000))
          pct <- as.integer(round(s / wait_sec * 100))
          phase <- if (pct < 50L) "Warmup" else "Sampling"

          line <- sprintf(
            "Chain %d Iteration: %d / 1000 [%3d%%] (%s)",
            chain_id,
            iter,
            pct,
            phase
          )
          # Write to both stderr and stdout to avoid backend-specific buffering.
          message(line)
          cat(line, "\n", sep = "")
          flush(stdout())
        }
      } else {
        # Standard task mode: emit taskr progress protocol lines.
        for (s in seq_len(wait_sec)) {
          Sys.sleep(1)
          cat(
            sprintf(
              "##TASKR_PROGRESS##{\"fraction\":%.6f,\"message\":\"task_%02d sleeping %d/%d sec\",\"ts\":\"%s\"}##/TASKR_PROGRESS##\n",
              s / wait_sec,
              idx,
              s,
              wait_sec,
              format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
            )
          )
          flush(stdout())
        }
      }

      sprintf("task_%02d done after %d sec", idx, wait_sec)
    },
    label = sprintf("demo_%02d", idx),
    priority = if (idx %% 3 == 0) 2L else 1L,
    import = list(
      env = c("idx", "wait_sec", "is_fake_model", "force_all_fake_model"),
      packages = character(),
      workdir = getwd()
    )
  )
}

message("Demo tasks submitted.")
