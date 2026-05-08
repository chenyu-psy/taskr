make_control_req <- function(path, payload = list(), method = "POST") {
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  state <- new.env(parent = emptyenv())
  state$done <- FALSE

  list(
    REQUEST_METHOD = method,
    PATH_INFO = path,
    rook.input = list(
      read = function() {
        if (isTRUE(state$done)) {
          return(raw())
        }
        state$done <- TRUE
        charToRaw(body)
      }
    )
  )
}

make_control_server_item <- function(id, label, status) {
  list(
    id = id,
    label = label,
    status = status,
    priority = 0L,
    resources = list(slots = 1L),
    output = "all",
    submit_time = Sys.time(),
    start_time = Sys.time(),
    end_time = as.POSIXct(NA),
    result_path = NULL,
    task = NULL,
    error = NULL
  )
}

make_control_server_running_task <- function() {
  state <- new.env(parent = emptyenv())
  state$cancelled <- FALSE

  list(
    error = NULL,
    status = function() "running",
    progress = function() NULL,
    read_output = function() "",
    read_error = function() "",
    is_alive = function() !state$cancelled,
    kill = function() {
      state$cancelled <- TRUE
      invisible(NULL)
    },
    elapsed = function() 0,
    state = state
  )
}

test_that("control server health endpoint returns running status", {
  handle_control_request <- getFromNamespace("handle_control_request", "taskr")

  res <- handle_control_request(make_control_req("/health", method = "GET"))
  body <- jsonlite::fromJSON(res$body, simplifyVector = TRUE)

  expect_equal(res$status, 200L)
  expect_true(body$ok)
  expect_identical(body$status, "running")
})

test_that("control server starts and stops cleanly", {
  skip_if_not_installed("httpuv")
  start_control_server <- getFromNamespace("start_control_server", "taskr")
  stop_control_server <- getFromNamespace("stop_control_server", "taskr")
  control_server_is_running <- getFromNamespace("control_server_is_running", "taskr")
  control_service_loop_is_running <- getFromNamespace("control_service_loop_is_running", "taskr")
  control_server_url <- getFromNamespace("control_server_url", "taskr")

  stop_control_server()
  on.exit(stop_control_server(), add = TRUE)

  url <- start_control_server()

  expect_true(control_server_is_running())
  expect_true(control_service_loop_is_running())
  expect_match(url, "^http://127[.]0[.]0[.]1:[0-9]+$")
  expect_identical(control_server_url(), url)

  stop_control_server()
  expect_false(control_server_is_running())
  expect_false(control_service_loop_is_running())
})

test_that("control server rejects invalid token", {
  handle_control_request <- getFromNamespace("handle_control_request", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")

  pkg_env$control_token <- "expected-token"
  res <- handle_control_request(make_control_req("/cancel", list(token = "wrong", task_id = "task_001")))
  body <- jsonlite::fromJSON(res$body, simplifyVector = TRUE)

  expect_equal(res$status, 500L)
  expect_false(body$ok)
  expect_match(body$error, "Invalid dashboard control token")
})

test_that("control server cancel endpoint updates main console scheduler", {
  handle_control_request <- getFromNamespace("handle_control_request", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$control_token <- "control-token"
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$queue <- list(
    make_control_server_item("task_control_001", "control_queued", "queued")
  )

  res <- handle_control_request(make_control_req(
    "/cancel",
    list(token = "control-token", task_id = "task_control_001")
  ))
  body <- jsonlite::fromJSON(res$body, simplifyVector = TRUE)

  expect_equal(res$status, 200L)
  expect_true(body$ok)
  expect_identical(body$status, "cancelled")
  expect_length(pkg_env$scheduler$queue, 0)
  expect_identical(pkg_env$scheduler$finished$task_control_001$status, "cancelled")
})

test_that("control server clean_finished endpoint removes finished records", {
  handle_control_request <- getFromNamespace("handle_control_request", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$control_token <- "control-token"
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    task_control_010 = make_control_server_item("task_control_010", "done", "completed"),
    task_control_011 = make_control_server_item("task_control_011", "failed", "failed")
  )

  res <- handle_control_request(make_control_req(
    "/clean_finished",
    list(token = "control-token")
  ))
  body <- jsonlite::fromJSON(res$body, simplifyVector = TRUE)

  expect_equal(res$status, 200L)
  expect_true(body$ok)
  expect_identical(body$action, "clean_finished")
  expect_equal(body$removed, 2L)
  expect_length(pkg_env$scheduler$finished, 0)
})

test_that("control server remove endpoint removes one failed task", {
  handle_control_request <- getFromNamespace("handle_control_request", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$control_token <- "control-token"
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$finished <- list(
    task_control_020 = make_control_server_item("task_control_020", "failed", "failed"),
    task_control_021 = make_control_server_item("task_control_021", "done", "completed")
  )
  pkg_env$scheduler$label_index <- list(
    failed = "task_control_020",
    done = "task_control_021"
  )

  res <- handle_control_request(make_control_req(
    "/remove",
    list(token = "control-token", task_id = "task_control_020")
  ))
  body <- jsonlite::fromJSON(res$body, simplifyVector = TRUE)

  expect_equal(res$status, 200L)
  expect_true(body$ok)
  expect_identical(body$action, "remove")
  expect_false("task_control_020" %in% names(pkg_env$scheduler$finished))
  expect_true("task_control_021" %in% names(pkg_env$scheduler$finished))
  expect_null(pkg_env$scheduler$label_index$failed)
  expect_identical(pkg_env$scheduler$label_index$done, "task_control_021")
})

test_that("control server remove endpoint cancels and removes one queued task", {
  handle_control_request <- getFromNamespace("handle_control_request", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$control_token <- "control-token"
  keep_item <- make_control_server_item("task_control_031", "queued_keep", "queued")
  keep_item$start_task <- function(item) make_control_server_running_task()
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$queue <- list(
    make_control_server_item("task_control_030", "queued_remove", "queued"),
    keep_item
  )
  pkg_env$scheduler$label_index <- list(
    queued_remove = "task_control_030",
    queued_keep = "task_control_031"
  )

  res <- handle_control_request(make_control_req(
    "/remove",
    list(token = "control-token", task_id = "task_control_030")
  ))
  body <- jsonlite::fromJSON(res$body, simplifyVector = TRUE)

  remaining_ids <- names(pkg_env$scheduler$running %||% list())
  remaining_ids <- c(remaining_ids, vapply(pkg_env$scheduler$queue, function(item) item$id, character(1)))
  remaining_ids <- c(remaining_ids, names(pkg_env$scheduler$finished %||% list()))
  expect_equal(res$status, 200L)
  expect_true(body$ok)
  expect_identical(body$action, "remove")
  expect_false("task_control_030" %in% remaining_ids)
  expect_true("task_control_031" %in% remaining_ids)
  expect_null(pkg_env$scheduler$label_index$queued_remove)
  expect_identical(pkg_env$scheduler$label_index$queued_keep, "task_control_031")
})
