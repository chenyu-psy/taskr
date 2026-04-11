# Overview

`taskr` is a lightweight background task manager for R. It runs long jobs in
separate R processes while keeping your main session responsive.

Current version focus:

- submit tasks (`submit_task()`, `submit_call()`, `map_calls()`)
- monitor tasks with the built-in Shiny **Task Monitor** (auto-launched)
- control tasks (`cancel_task()`, `clean_tasks()`)
- retrieve logs/results (`task_logs()`, `task_result()`)

The queue is session-local and temporary by design: restarting R clears queue
state and task records.

# Installation

Install from GitHub:

```r
install.packages("remotes")
remotes::install_github("chenyu-psy/taskr")
```

# Quick Start

```r
library(taskr)

# Optional: set concurrency once
init_queue(max_concurrent = 3)

# Submit a task (Task Monitor auto-opens in Viewer when available)
submit_task(
  expr = {
    Sys.sleep(10)
    "done"
  },
  label = "demo"
)

# Query status any time
list_tasks()
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
```

Retrieve logs and results:

```r
task_logs("brm_model")    # stdout/stderr from the child process
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

### 2. Task Monitor (Shiny)

`taskr` now uses a Shiny dashboard (**Task Monitor**) as the default live
monitoring interface.

Behavior in the current version:

- dashboard auto-launches after `submit_*` calls (interactive sessions)
- dashboard runs in a background process, so console stays usable
- console prints `Listening on http://127.0.0.1:...` for browser access
- set `options(taskr.auto_dashboard = FALSE)` to disable auto-launch

Task Monitor layout:

- **Summary**: slot usage and terminal completion progress bars
- **Running**: active tasks with elapsed time, cancel action, and progress bars
  (including per-chain bars for Stan/JAGS-style logs when detected)
- **Queued**: waiting tasks sorted by priority then submit time
- **Finished**: terminal tasks with filters (`done` / `failed` / `killed`) and
  `Clean Finished`
- click a task card to expand details and tail logs

A ready-to-run demo is available at `inst/examples/shiny-loop-demo.R`:

```r
source(system.file("examples", "shiny-loop-demo.R", package = "taskr"))
```

# Notes

- The scheduler starts lazily when tasks are submitted and stops when the queue
  is empty.
- `import = "auto"` (the default) automatically captures referenced variables
  and loaded packages from the calling environment.
- Task status values: `"queued"`, `"running"`, `"done"`, `"failed"`, `"killed"`.
- Results are stored as `.rds` files in a temporary directory and are lost
  when the R session ends.
- Dashboard snapshots are temporary session files used for background monitor
  reads. They are not long-term records.
- `list_tasks()` returns a data frame with columns: `id`, `label`, `status`,
  `progress`, `message`, `elapsed`, `error`, `submit_time`, `start_time`,
  `end_time`.
- The Task Monitor requires `shiny` and background launch requires `callr`
  (both listed in Suggests).

# Status

`taskr` is under active development. The core end-to-end workflow is available
for practical testing. Feedback and contributions are welcome.
