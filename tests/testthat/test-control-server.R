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
