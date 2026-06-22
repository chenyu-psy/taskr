#' Internal Helpers to Launch Background Tasks
#'
#' This file contains the first subprocess launcher used by `taskr`. The helper
#' builds a wrapped expression that evaluates user code in a child R session and
#' optionally saves the result to disk. The launcher returns an internal `Task`
#' object so later queue code can monitor the running process.

#' Build a wrapped expression for a background task.
#'
#' Purpose:
#' - Create the exact expression that the child R session should evaluate.
#'
#' Parameters:
#' - `expr`: User expression to run in the child process.
#' - `result_path`: Optional file path where the child should save its result.
#' - `status_path`: Optional file path where the child should write task
#'   lifecycle status markers.
#' - `imports`: Named list of objects to assign into the child global
#'   environment before evaluating `expr`.
#' - `packages`: Optional character vector of package names to load in the
#'   child process before evaluating `expr`.
#' - `workdir`: Optional working directory for the child process.
#' - `env`: Optional named list of environment variables for the child process.
#'
#' Returns:
#' - An R language object suitable for submission to `callr::r_session`.
#'
#' Assumptions and side effects:
#' - The expression is evaluated in the child process, not in the caller.
#' - When `result_path` is provided, the child writes one `.rds` file.
#' - When `status_path` is provided, the child writes one status marker file.
#' - Package loading, working directory changes, and environment variable
#'   updates happen in the child process only.
#'
#' @keywords internal
build_task_expr <- function(
    expr,
    result_path = NULL,
    status_path = NULL,
    imports = list(),
    packages = NULL,
    workdir = NULL,
    env = NULL) {
  if (!is.null(result_path)) {
    if (!is.character(result_path) || length(result_path) != 1 || is.na(result_path)) {
      stop("`result_path` must be NULL or a single non-missing character string.")
    }
  }

  if (!is.null(status_path)) {
    if (!is.character(status_path) || length(status_path) != 1 || is.na(status_path)) {
      stop("`status_path` must be NULL or a single non-missing character string.")
    }
  }

  if (!is.list(imports)) {
    stop("`imports` must be a named list.")
  }
  if (length(imports) > 0 && (is.null(names(imports)) || any(!nzchar(names(imports))))) {
    stop("`imports` must be a named list.")
  }

  if (!is.null(packages)) {
    if (!is.character(packages) || any(is.na(packages))) {
      stop("`packages` must be NULL or a character vector without missing values.")
    }
  }

  if (!is.null(workdir)) {
    if (!is.character(workdir) || length(workdir) != 1 || is.na(workdir) || !nzchar(workdir)) {
      stop("`workdir` must be NULL or a single non-empty character string.")
    }
  }

  if (!is.null(env)) {
    if (!is.list(env) || is.null(names(env)) || any(!nzchar(names(env)))) {
      stop("`env` must be NULL or a named list.")
    }
  }

  bquote({
    .__taskr_write_status <- function(status, message = NULL) {
      .__taskr_status_path <- .(status_path)
      if (is.null(.__taskr_status_path)) {
        return(invisible(NULL))
      }

      .__taskr_payload <- list(
        status = status,
        message = message,
        time = Sys.time()
      )
      .__taskr_tmp <- paste0(
        .__taskr_status_path,
        ".tmp-",
        Sys.getpid(),
        "-",
        as.integer(runif(1, 1, .Machine$integer.max))
      )
      saveRDS(.__taskr_payload, .__taskr_tmp)
      if (!file.rename(.__taskr_tmp, .__taskr_status_path)) {
        unlink(.__taskr_tmp, force = TRUE)
        stop("Failed to write task status marker.")
      }

      invisible(NULL)
    }

    if (length(.(packages)) > 0) {
      for (.__taskr_pkg in .(packages)) {
        library(.__taskr_pkg, character.only = TRUE)
      }
    }

    if (!is.null(.(workdir))) {
      setwd(.(workdir))
    }

    if (!is.null(.(env))) {
      do.call(Sys.setenv, as.list(.(env)))
    }

    if (length(.(imports)) > 0) {
      list2env(.(imports), envir = .GlobalEnv)
    }

    tryCatch({
      .__taskr_write_status("running")
      .__taskr_result <- .(expr)

      if (!is.null(.(result_path))) {
        saveRDS(.__taskr_result, .(result_path))
      }

      .__taskr_write_status("completed")
      invisible(NULL)
    }, error = function(.__taskr_error) {
      .__taskr_write_status("failed", conditionMessage(.__taskr_error))
      stop(.__taskr_error)
    })
  })
}

#' Launch a background task in a child R session.
#'
#' Purpose:
#' - Start one asynchronous child process and return a `Task` object that wraps
#'   the session handle.
#'
#' Parameters:
#' - `id`: Task identifier used by the package registry.
#' - `expr`: User expression to run in the child process.
#' - `result_path`: Optional path where the child should save its result.
#'   When `NULL`, the standard `task_tmpfile(id)` path is used.
#' - `import`: Optional character vector of object names to copy from the main
#'   session `.GlobalEnv` into the child process.
#' - `packages`: Optional character vector of package names to load in the
#'   child process.
#' - `workdir`: Optional working directory for the child process.
#' - `env`: Optional named list of environment variables to set in the child
#'   process.
#' - `save_result`: Whether task output should be written to result storage.
#'
#' Returns:
#' - A `Task` object in `running` state.
#'
#' Assumptions and side effects:
#' - Requires the `callr` package to be installed.
#' - Starts a new background R session immediately.
#' - The child process runs asynchronously after this function returns.
#'
#' @keywords internal
start_task_process <- function(
    id,
    expr,
    result_path = NULL,
    import = NULL,
    packages = NULL,
    workdir = NULL,
    env = NULL,
    save_result = TRUE) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop(
      "The `callr` package is required to start background tasks. ",
      "Please install it before using task subprocess features."
    )
  }

  if (!is.logical(save_result) || length(save_result) != 1 || is.na(save_result)) {
    stop("`save_result` must be a single non-missing logical value.")
  }

  if (isTRUE(save_result) && is.null(result_path)) {
    result_path <- task_tmpfile(id)
  } else if (!isTRUE(save_result)) {
    result_path <- NULL
  }
  status_path <- task_status_file(id)
  unlink(status_path, force = TRUE)

  imports <- resolve_task_imports(import = import)

  wrapped_expr <- build_task_expr(
    expr = expr,
    result_path = result_path,
    status_path = status_path,
    imports = imports,
    packages = packages,
    workdir = workdir,
    env = env
  )
  # Use r_bg with stdout/stderr pipes so running tasks can stream logs
  # progressively into dashboard views.
  session <- callr::r_bg(
    func = function(code) {
      eval(code, envir = .GlobalEnv)
      invisible(NULL)
    },
    args = list(code = wrapped_expr),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )

  task <- Task$new(
    id = id,
    process = session,
    status = "running",
    result_path = result_path,
    status_path = status_path
  )
  register_active_task(task)
  task
}

#' Resolve imported objects for a child task.
#'
#' Purpose:
#' - Copy a named subset of objects from the current `.GlobalEnv` so the child
#'   process can evaluate expressions that depend on them.
#'
#' Parameters:
#' - `import`: Optional character vector of object names to copy.
#'
#' Returns:
#' - A named list of imported objects.
#'
#' Assumptions and side effects:
#' - Reads objects from `.GlobalEnv`.
#' - Errors clearly if a requested object does not exist.
#'
#' @keywords internal
resolve_task_imports <- function(import = NULL) {
  if (is.null(import)) {
    return(list())
  }

  if (!is.character(import) || any(is.na(import)) || any(!nzchar(import))) {
    stop("`import` must be NULL or a character vector of non-empty object names.")
  }

  missing_names <- import[!vapply(import, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)]
  if (length(missing_names) > 0) {
    stop(
      "Imported object(s) not found in `.GlobalEnv`: ",
      paste(missing_names, collapse = ", ")
    )
  }

  stats::setNames(
    lapply(import, get, envir = .GlobalEnv, inherits = FALSE),
    import
  )
}
