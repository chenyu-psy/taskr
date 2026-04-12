# Shiny Dashboard Demo Script
#
# Purpose:
# - Submit a small batch of background tasks with progress updates.
# - Open `launch_dashboard()` so users can visually verify queue behavior.
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
taskr::init_queue(max_slots = 1)

demo_conditions <- c(
  "chain_stan_like",
  "chain_fraction",
  "chain_percent",
  "fraction_step",
  "fraction_of",
  "percent_plain",
  "visual_bar",
  "freeze_ambiguous_after_valid"
)

# Each demo runs for 10-20 seconds to keep the dashboard easy to inspect.
wait_plan <- sample(10:20, size = length(demo_conditions), replace = TRUE)
# Stagger submissions so new tasks appear gradually in the dashboard.
submit_gap_plan <- sample(3:5, size = length(demo_conditions) - 1L, replace = TRUE)

for (i in seq_along(demo_conditions)) {
  idx <- as.integer(i)
  condition <- demo_conditions[[i]]
  wait_sec <- as.integer(wait_plan[[i]])

  taskr::submit_code(
    expr = {
      for (s in seq_len(wait_sec)) {
        Sys.sleep(1)

        if (identical(condition, "chain_stan_like")) {
          chains <- 4L
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
          message(line)
          cat(line, "\n", sep = "")
        } else if (identical(condition, "chain_fraction")) {
          chain_id <- ((s - 1L) %% 3L) + 1L
          line <- sprintf("Chain %d step %d/%d", chain_id, s, wait_sec)
          cat(line, "\n", sep = "")
        } else if (identical(condition, "chain_percent")) {
          chain_id <- ((s - 1L) %% 3L) + 1L
          pct <- as.integer(round(s / wait_sec * 100))
          line <- sprintf("Chain %d: progress %d%%", chain_id, pct)
          cat(line, "\n", sep = "")
        } else if (identical(condition, "fraction_step")) {
          line <- sprintf("step %d/%d", s, wait_sec)
          cat(line, "\n", sep = "")
        } else if (identical(condition, "fraction_of")) {
          line <- sprintf("iteration %d of %d", s, wait_sec)
          cat(line, "\n", sep = "")
        } else if (identical(condition, "percent_plain")) {
          pct <- as.integer(round(s / wait_sec * 100))
          line <- sprintf("Progress: %d%%", pct)
          cat(line, "\n", sep = "")
        } else if (identical(condition, "visual_bar")) {
          width <- 20L
          filled <- as.integer(round(s / wait_sec * width))
          line <- sprintf(
            "[%s%s]",
            paste0(rep("#", filled), collapse = ""),
            paste0(rep("-", width - filled), collapse = "")
          )
          cat(line, "\n", sep = "")
        } else if (identical(condition, "freeze_ambiguous_after_valid")) {
          if (s <= (wait_sec %/% 3L)) {
            line <- sprintf("Progress: %d%%", as.integer(round(s / wait_sec * 100)))
          } else {
            line <- sprintf("accuracy %d%%", as.integer(round(s / wait_sec * 100)))
          }
          cat(line, "\n", sep = "")
        }

        flush(stdout())
      }

      sprintf("task_%02d (%s) completed after %d sec", idx, condition, wait_sec)
    },
    label = sprintf("demo%02d_%s", idx, condition),
    priority = 1L,
    import = list(
      env = c("idx", "condition", "wait_sec"),
      packages = character(),
      workdir = getwd()
    )
  )

  if (i < length(demo_conditions)) {
    gap_sec <- as.integer(submit_gap_plan[[i]])
    message(sprintf("Submitted %s; waiting %d sec before next submit.", condition, gap_sec))
    Sys.sleep(gap_sec)
  }
}

message("Demo tasks submitted.")
