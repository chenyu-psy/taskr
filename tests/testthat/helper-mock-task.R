# Mock Task Class for Queue-Layer Tests
#
# Purpose:
# - Provide a deterministic task object that satisfies the internal Task
#   contract without launching real subprocesses.
# - Let scheduler tests focus on state transitions instead of backend details.

MockTask <- R6::R6Class(
  classname = "MockTask",
  public = list(
    id = NULL,
    created_at = NULL,
    status_value = NULL,
    progress_state = NULL,
    output_queue = NULL,
    error_queue = NULL,
    stdout_buffer = NULL,
    stderr_buffer = NULL,
    finished_at = NULL,

    initialize = function(
        id,
        status = "running",
        output_queue = character(),
        error_queue = character(),
        progress_state = NULL) {
      if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
        stop("`id` must be a single non-empty character string.")
      }

      allowed_status <- c("running", "completed", "failed", "cancelled")
      if (!status %in% allowed_status) {
        stop("`status` must be one of: ", paste(allowed_status, collapse = ", "))
      }

      self$id <- id
      self$created_at <- Sys.time()
      self$status_value <- status
      self$progress_state <- progress_state
      self$output_queue <- output_queue
      self$error_queue <- error_queue
      self$stdout_buffer <- ""
      self$stderr_buffer <- ""

      if (status %in% c("completed", "failed", "cancelled")) {
        self$finished_at <- self$created_at
      }
    },

    status = function() {
      self$status_value
    },

    set_status = function(status) {
      allowed_status <- c("running", "completed", "failed", "cancelled")
      if (!status %in% allowed_status) {
        stop("`status` must be one of: ", paste(allowed_status, collapse = ", "))
      }

      self$status_value <- status
      if (status %in% c("completed", "failed", "cancelled")) {
        self$finished_at <- Sys.time()
      }

      invisible(self)
    },

    is_alive = function() {
      identical(self$status_value, "running")
    },

    progress = function() {
      self$progress_state
    },

    set_progress = function(fraction, message = NULL) {
      self$progress_state <- list(
        fraction = fraction,
        message = message,
        updated_at = Sys.time()
      )

      invisible(self)
    },

    read_output = function() {
      if (length(self$output_queue) == 0) {
        return("")
      }

      chunk <- self$output_queue[[1]]
      self$output_queue <- self$output_queue[-1]
      self$stdout_buffer <- paste0(self$stdout_buffer, chunk)
      chunk
    },

    read_error = function() {
      if (length(self$error_queue) == 0) {
        return("")
      }

      chunk <- self$error_queue[[1]]
      self$error_queue <- self$error_queue[-1]
      self$stderr_buffer <- paste0(self$stderr_buffer, chunk)
      chunk
    },

    kill = function() {
      self$set_status("cancelled")
    },

    elapsed = function() {
      end_time <- self$finished_at
      if (is.null(end_time)) {
        end_time <- Sys.time()
      }

      as.numeric(difftime(end_time, self$created_at, units = "secs"))
    }
  )
)
