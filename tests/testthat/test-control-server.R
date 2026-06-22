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
  res <- handle_control_request(make_control_req("/cancel", list(token = "wrong", task_id = 1L)))
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
  pkg_env$scheduler$pending <- list(
    make_control_server_item(1L, "control_pending", "pending")
  )

  res <- handle_control_request(make_control_req(
    "/cancel",
    list(token = "control-token", task_id = 1L)
  ))
  body <- jsonlite::fromJSON(res$body, simplifyVector = TRUE)

  expect_equal(res$status, 200L)
  expect_true(body$ok)
  expect_identical(body$status, "cancelled")
  expect_length(pkg_env$scheduler$pending, 0)
  expect_identical(pkg_env$scheduler$finished[["1"]]$status, "cancelled")
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
    "1" = make_control_server_item(1L, "done", "completed"),
    "2" = make_control_server_item(2L, "failed", "failed")
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
    "1" = make_control_server_item(1L, "failed", "failed"),
    "2" = make_control_server_item(2L, "done", "completed")
  )

  res <- handle_control_request(make_control_req(
    "/remove",
    list(token = "control-token", task_id = 1L)
  ))
  body <- jsonlite::fromJSON(res$body, simplifyVector = TRUE)

  expect_equal(res$status, 200L)
  expect_true(body$ok)
  expect_identical(body$action, "remove")
  expect_false("1" %in% names(pkg_env$scheduler$finished))
  expect_true("2" %in% names(pkg_env$scheduler$finished))
})

test_that("control server remove endpoint cancels and removes one pending task", {
  handle_control_request <- getFromNamespace("handle_control_request", "taskr")
  pkg_env <- getFromNamespace("pkg_env", "taskr")
  new_scheduler_state <- getFromNamespace("new_scheduler_state", "taskr")

  taskr::shutdown_queue()
  taskr::init_queue(max_slots = 1)
  on.exit(taskr::shutdown_queue(), add = TRUE)

  pkg_env$control_token <- "control-token"
  keep_item <- make_control_server_item(2L, "pending_keep", "pending")
  keep_item$start_task <- function(item) make_control_server_running_task()
  pkg_env$scheduler <- new_scheduler_state(max_slots = 1)
  pkg_env$scheduler$pending <- list(
    make_control_server_item(1L, "pending_remove", "pending"),
    keep_item
  )

  res <- handle_control_request(make_control_req(
    "/remove",
    list(token = "control-token", task_id = 1L)
  ))
  body <- jsonlite::fromJSON(res$body, simplifyVector = TRUE)

  remaining_ids <- names(pkg_env$scheduler$running %||% list())
  remaining_ids <- c(remaining_ids, vapply(pkg_env$scheduler$pending, function(item) as.character(item$id), character(1)))
  remaining_ids <- c(remaining_ids, names(pkg_env$scheduler$finished %||% list()))
  expect_equal(res$status, 200L)
  expect_true(body$ok)
  expect_identical(body$action, "remove")
  expect_false("1" %in% remaining_ids)
  expect_true("2" %in% remaining_ids)
})
