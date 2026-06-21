make_fake_process <- function(alive = TRUE) {
  state <- new.env(parent = emptyenv())
  state$alive <- alive
  state$kill_calls <- 0L

  list(
    is_alive = function() {
      state$alive
    },
    kill = function() {
      state$kill_calls <- state$kill_calls + 1L
      state$alive <- FALSE
      invisible(NULL)
    },
    state = state
  )
}

make_fake_io_process <- function(output = character(), error = character()) {
  state <- new.env(parent = emptyenv())
  state$alive <- TRUE
  state$output <- output
  state$error <- error

  list(
    is_alive = function() {
      state$alive
    },
    kill = function() {
      state$alive <- FALSE
      invisible(NULL)
    },
    read_output = function() {
      if (length(state$output) == 0) {
        return("")
      }

      value <- state$output[[1]]
      state$output <- state$output[-1]
      value
    },
    read_error = function() {
      if (length(state$error) == 0) {
        return("")
      }

      value <- state$error[[1]]
      state$error <- state$error[-1]
      value
    }
  )
}

make_fake_terminal_process <- function(alive = FALSE, error = character(), exit_status = NULL) {
  state <- new.env(parent = emptyenv())
  state$alive <- alive
  state$error <- error
  state$kill_calls <- 0L
  state$exit_status <- exit_status

  list(
    is_alive = function() {
      state$alive
    },
    kill = function() {
      state$kill_calls <- state$kill_calls + 1L
      state$alive <- FALSE
      invisible(NULL)
    },
    read_output = function() {
      ""
    },
    read_error = function() {
      if (length(state$error) == 0) {
        return("")
      }

      value <- state$error[[1]]
      state$error <- state$error[-1]
      value
    },
    get_exit_status = function() {
      state$exit_status
    },
    state = state
  )
}

test_that("Task stores basic metadata", {
  task <- taskr:::Task$new(id = 1L)

  expect_equal(task$id, 1L)
  expect_equal(task$status(), "pending")
  expect_false(task$is_alive())
  expect_s3_class(task$created_at, "POSIXct")
})

test_that("Task validates id and status inputs", {
  expect_error(taskr:::Task$new(id = ""), "positive integer")
  expect_error(taskr:::Task$new(id = NA_character_), "positive integer")
  expect_error(taskr:::Task$new(id = 1L, status = "unknown"), "must be one of")
})

test_that("Task uses the process handle to report liveness", {
  fake_process <- make_fake_process(alive = TRUE)
  task <- taskr:::Task$new(
    id = 2L,
    process = fake_process,
    status = "running"
  )

  expect_true(task$is_alive())
  fake_process$state$alive <- FALSE
  expect_false(task$is_alive())
})

test_that("Task kill updates status and stops the process", {
  fake_process <- make_fake_process(alive = TRUE)
  task <- taskr:::Task$new(
    id = 3L,
    process = fake_process,
    status = "running"
  )

  task$kill()

  expect_equal(task$status(), "cancelled")
  expect_equal(fake_process$state$kill_calls, 1L)
  expect_false(task$is_alive())
  expect_s3_class(task$finished_at, "POSIXct")
})

test_that("Task kill errors if the process stays alive", {
  fake_process <- make_fake_process(alive = TRUE)
  fake_process$kill <- function() {
    fake_process$state$kill_calls <- fake_process$state$kill_calls + 1L
    invisible(NULL)
  }
  task <- taskr:::Task$new(
    id = 12L,
    process = fake_process,
    status = "running"
  )

  expect_error(task$kill(), "still alive")
  expect_equal(fake_process$state$kill_calls, 1L)
  expect_equal(task$status(), "running")
})

test_that("Task status keeps cancelled result from becoming completed", {
  fake_process <- make_fake_process(alive = FALSE)
  result_path <- tempfile(fileext = ".rds")
  saveRDS(1L, result_path)
  task <- taskr:::Task$new(
    id = 13L,
    process = fake_process,
    status = "running",
    result_path = result_path
  )
  task$cancel_requested <- TRUE

  expect_equal(task$status(), "cancelled")
  expect_false(file.exists(result_path))
})

test_that("Task elapsed returns a non-negative number", {
  task <- taskr:::Task$new(id = 4L)

  expect_type(task$elapsed(), "double")
  expect_gte(task$elapsed(), 0)
})

test_that("Task read_output keeps ordinary stdout text", {
  fake_process <- make_fake_io_process(output = c("hello\n", "world\n"))
  task <- taskr:::Task$new(id = 5L, process = fake_process, status = "running")

  first_chunk <- task$read_output()
  second_chunk <- task$read_output()

  expect_identical(first_chunk, "hello\n")
  expect_identical(second_chunk, "world\n")
  expect_identical(task$stdout_buffer, "hello\nworld\n")
  expect_null(task$progress())
})

test_that("Task read_output parses tagged progress events", {
  progress_line <- paste0(
    "before\n",
    "##TASKR_PROGRESS##",
    "{\"fraction\":0.25,\"message\":\"step 1\",\"ts\":\"2026-04-09T14:23:11Z\"}",
    "##/TASKR_PROGRESS##",
    "\nafter\n"
  )
  fake_process <- make_fake_io_process(output = progress_line)
  task <- taskr:::Task$new(id = 6L, process = fake_process, status = "running")

  chunk <- task$read_output()
  progress <- task$progress()

  expect_identical(chunk, "before\n\nafter\n")
  expect_identical(task$stdout_buffer, "before\n\nafter\n")
  expect_equal(progress$fraction, 0.25)
  expect_equal(progress$message, "step 1")
  expect_s3_class(progress$updated_at, "POSIXct")
})

test_that("Task read_output keeps the latest progress event", {
  progress_line <- paste0(
    "##TASKR_PROGRESS##",
    "{\"fraction\":0.10,\"message\":\"step 1\",\"ts\":\"2026-04-09T14:23:11Z\"}",
    "##/TASKR_PROGRESS##",
    "##TASKR_PROGRESS##",
    "{\"fraction\":0.90,\"message\":\"step 9\",\"ts\":\"2026-04-09T14:23:19Z\"}",
    "##/TASKR_PROGRESS##"
  )
  fake_process <- make_fake_io_process(output = progress_line)
  task <- taskr:::Task$new(id = 7L, process = fake_process, status = "running")

  task$read_output()
  progress <- task$progress()

  expect_equal(progress$fraction, 0.90)
  expect_equal(progress$message, "step 9")
})

test_that("Task read_error appends stderr text", {
  fake_process <- make_fake_io_process(error = c("warn 1\n", "warn 2\n"))
  task <- taskr:::Task$new(id = 8L, process = fake_process, status = "running")

  first_chunk <- task$read_error()
  second_chunk <- task$read_error()

  expect_identical(first_chunk, "warn 1\n")
  expect_identical(second_chunk, "warn 2\n")
  expect_identical(task$stderr_buffer, "warn 1\nwarn 2\n")
})

test_that("Task status becomes completed when the result file exists", {
  result_path <- tempfile(fileext = ".rds")
  saveRDS("completed", result_path)
  fake_process <- make_fake_terminal_process(alive = FALSE)
  task <- taskr:::Task$new(
    id = 9L,
    process = fake_process,
    status = "running",
    result_path = result_path
  )
  taskr:::register_active_task(task)

  expect_equal(task$status(), "completed")
  expect_false(exists("9", envir = taskr:::pkg_env$active_tasks, inherits = FALSE))

  unlink(result_path)
})

test_that("Task status completes when no result file is expected", {
  fake_process <- make_fake_terminal_process(alive = FALSE)
  task <- taskr:::Task$new(
    id = 14L,
    process = fake_process,
    status = "running",
    result_path = NULL
  )
  taskr:::register_active_task(task)

  expect_equal(task$status(), "completed")
  expect_false(exists("14", envir = taskr:::pkg_env$active_tasks, inherits = FALSE))
  expect_null(task$error)
})

test_that("Task status becomes failed when the process exits without a result file", {
  fake_process <- make_fake_terminal_process(alive = FALSE, error = "boom from child\n")
  task <- taskr:::Task$new(
    id = 10L,
    process = fake_process,
    status = "running",
    result_path = tempfile(fileext = ".rds")
  )

  expect_equal(task$status(), "failed")
  expect_match(task$error, "boom from child")
})

test_that("Task status becomes failed when an output-none process exits with an error", {
  fake_process <- make_fake_terminal_process(
    alive = FALSE,
    error = "child process failed\n",
    exit_status = 1L
  )
  task <- taskr:::Task$new(
    id = 15L,
    process = fake_process,
    status = "running",
    result_path = NULL
  )

  expect_equal(task$status(), "failed")
  expect_match(task$error, "child process failed")
})

test_that("Task status uses a fallback failure message when stderr is empty", {
  fake_process <- make_fake_terminal_process(alive = FALSE)
  task <- taskr:::Task$new(
    id = 11L,
    process = fake_process,
    status = "running",
    result_path = tempfile(fileext = ".rds")
  )

  expect_equal(task$status(), "failed")
  expect_match(task$error, "subprocess died unexpectedly")
})
