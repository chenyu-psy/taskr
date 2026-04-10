# Overview

This package provides a lightweight background task manager for R. It runs
long-running jobs in separate R processes while keeping the main R session
responsive. Built on a queue-based scheduler, it supports task submission,
real-time progress reporting, and resource-aware scheduling.

The package is editor-agnostic and works in base R, RStudio, Positron, or any
other R environment. It is designed for research workflows involving model
fitting, simulations, and other computationally intensive tasks.

# Installation

Install from GitHub:

```r
install.packages("remotes")
remotes::install_github("chenyu-psy/taskr")
```

# Quick Start

```r
library(taskr)

# Initialize a queue with 2 concurrent slots
init_queue(max_concurrent = 2)

# Submit a background task
submit_task(
  expr = {
    Sys.sleep(5)
    "done"
  },
  label = "demo"
)

# Check status
list_tasks(label = "demo")

# Retrieve result (blocking)
task_result("demo")

# Clean up
clean_tasks()
shutdown_queue()
```

# Task Manager

The task manager provides background execution plus queue-based scheduling.
Tasks are submitted to a priority queue and launched automatically when
slots become available. The scheduler runs non-blockingly via `later`,
so it integrates naturally with Shiny and other event-driven R frameworks.

## Core Functions

### 1. Submitting Background Tasks

The `submit_task` function executes an R expression in a background process
with full control over variable imports, priority, and resource allocation:

```r
submit_task(
  expr = {
    model <- lm(mpg ~ wt + qsec, data = mtcars)
    summary(model)
  },
  label = "lm_fit",
  priority = 1          # higher priority runs first
)
```

The `submit_call` function executes a function call with explicit arguments:

```r
submit_call(
  fun = brms::brm,
  args = list(
    formula = mpg ~ wt + qsec,
    data = mtcars,
    chains = 4
  ),
  label = "brm_model"
)
```

The `map_calls` function batch-submits tasks over a parameter grid, which is
useful for running the same function with different parameter combinations:

```r
grid <- data.frame(
  k = c(1, 2, 3),
  n = c(100, 200, 300)
)

map_calls(
  fun = function(k, n) rnorm(n, mean = k),
  grid = grid,
  label_fmt = "sim_k{k}_n{n}"
)
```

### 2. Queue Management

Initialize the queue and set concurrency limits. By default, the number of
concurrent slots is auto-detected based on available CPU cores:

```r
init_queue()              # auto-detect
init_queue(max_concurrent = 4)  # explicit
```

Shutdown the queue and kill all active tasks:

```r
shutdown_queue()
```

### 3. Monitoring and Querying

Track the progress and status of all submitted tasks:

```r
list_tasks()                     # view all tasks
list_tasks(status = "running")   # filter by status
list_tasks(label = "brm_model")  # filter by label

queue_overview()                 # compact summary of queue state
```

Retrieve logs and results:

```r
task_logs("brm_model")    # stdout/stderr captured from the child process
task_result("brm_model")  # blocking: waits until the task finishes
```

### 4. Task Control

Cancel a running or queued task, or clean up completed tasks:

```r
cancel_task("brm_model")   # kill running or remove queued task
clean_tasks()               # remove all done/failed/killed records
```

## Advanced Functions

### 1. Progress Reporting

The `report_progress` function can be called inside a background task to
report progress back to the main session:

```r
submit_task(
  expr = {
    for (i in 1:100) {
      Sys.sleep(0.1)
      taskr::report_progress(i / 100, sprintf("Step %d of 100", i))
    }
    "completed"
  },
  label = "long_job"
)

# Progress is visible in list_tasks()
list_tasks(label = "long_job")
```

### 2. Stan Progress Parsing

The `stan_progress` function parses CmdStan/RStan progress output from task
logs and displays compact per-chain progress bars:

```r
stan_progress("brm_model")
```

This prints per-chain text progress bars and returns a data frame with
columns `chain`, `progress`, and `phase`.

# Notes

- The scheduler lifecycle is automatic: it starts lazily when tasks are
  submitted and stops when the queue is empty.
- `import = "auto"` (the default) automatically captures referenced variables
  and loaded packages from the calling environment.
- Task status values: `"queued"`, `"running"`, `"done"`, `"failed"`, `"killed"`.
- Results are stored as `.rds` files in a temporary directory and are lost
  when the R session ends.
- `list_tasks()` returns a data frame with columns: `id`, `label`, `status`,
  `progress`, `message`, `elapsed`, `error`, `submit_time`, `start_time`,
  `end_time`.

# Status

`taskr` is under active development. The core end-to-end workflow is available
for practical testing. Feedback and contributions are welcome.
