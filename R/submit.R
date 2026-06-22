# Task Submission API (User-Facing)
#
# Purpose:
# - Provide simple user functions to enqueue expression tasks and function-call
#   tasks into the in-memory scheduler.
# - Keep submission logic explicit so later queue features remain easy to read.

validate_label <- function(label = NULL) {
  if (is.null(label)) {
    return(invisible(NULL))
  }

  if (!is.character(label) || length(label) != 1 || is.na(label) || !nzchar(label)) {
    stop("`label` must be NULL or a single non-empty character string.")
  }

  invisible(NULL)
}

validate_priority <- function(priority = 0L) {
  if (!is.numeric(priority) || length(priority) != 1 || is.na(priority)) {
    stop("`priority` must be a single numeric value.")
  }

  as.integer(priority)
}

normalize_resources <- function(resources = list(slots = 1L)) {
  if (!is.list(resources) || is.null(resources$slots)) {
    stop("`resources` must be a list with a `slots` field.")
  }

  slots <- resources$slots
  if (!is.numeric(slots) || length(slots) != 1 || is.na(slots)) {
    stop("`resources$slots` must be a single positive integer.")
  }

  slots <- as.integer(slots)
  if (slots < 1L) {
    stop("`resources$slots` must be >= 1.")
  }

  list(slots = slots)
}

auto_import_env_from_expr <- function(expr) {
  names <- all.names(expr, functions = FALSE, unique = TRUE)
  names[vapply(names, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)]
}

auto_import_env_from_fun <- function(fun) {
  if (!is.function(fun)) {
    return(character())
  }

  if (is.primitive(fun) || is.null(body(fun))) {
    return(character())
  }

  names <- all.names(body(fun), functions = FALSE, unique = TRUE)
  formal_names <- names(formals(fun))
  names <- setdiff(names, formal_names)
  names[vapply(names, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)]
}

auto_import_packages <- function() {
  attached <- grep("^package:", search(), value = TRUE)
  packages <- sub("^package:", "", attached)
  packages <- packages[!packages %in% c("base", "taskr")]
  packages <- packages[vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  packages[nzchar(vapply(packages, function(pkg) system.file(package = pkg), character(1)))]
}

resolve_submit_context <- function(import = "auto", expr = NULL, fun = NULL) {
  if (is.character(import) && length(import) == 1 && !is.na(import)) {
    if (identical(import, "none")) {
      return(list(
        env = character(),
        packages = character(),
        workdir = NULL,
        vars = NULL
      ))
    }

    if (identical(import, "auto")) {
      env_names <- character()
      if (!is.null(expr)) {
        env_names <- auto_import_env_from_expr(expr)
      } else if (!is.null(fun)) {
        env_names <- auto_import_env_from_fun(fun)
      }

      return(list(
        env = env_names,
        packages = auto_import_packages(),
        workdir = getwd(),
        vars = NULL
      ))
    }
  }

  if (!is.list(import)) {
    stop("`import` must be \"auto\", \"none\", or a list(env, packages, workdir, vars).")
  }

  allowed_fields <- c("env", "packages", "workdir", "vars")
  unknown_fields <- setdiff(names(import), allowed_fields)
  if (length(unknown_fields) > 0) {
    stop(
      "`import` list contains unknown field(s): ",
      paste(unknown_fields, collapse = ", "),
      ". Allowed fields are: env, packages, workdir, vars."
    )
  }

  env_names <- import$env %||% character()
  if (!is.character(env_names) || any(is.na(env_names)) || any(!nzchar(env_names))) {
    stop("`import$env` must be a character vector of non-empty object names.")
  }

  packages <- import$packages %||% character()
  if (!is.character(packages) || any(is.na(packages)) || any(!nzchar(packages))) {
    stop("`import$packages` must be a character vector of non-empty package names.")
  }

  workdir <- import$workdir %||% NULL
  if (!is.null(workdir)) {
    if (!is.character(workdir) || length(workdir) != 1 || is.na(workdir) || !nzchar(workdir)) {
      stop("`import$workdir` must be NULL or a single non-empty character string.")
    }
  }

  vars <- import$vars %||% NULL
  if (!is.null(vars)) {
    if (!is.list(vars) || is.null(names(vars)) || any(!nzchar(names(vars)))) {
      stop("`import$vars` must be NULL or a named list.")
    }
  }

  list(
    env = env_names,
    packages = packages,
    workdir = workdir,
    vars = vars
  )
}

#' Normalize output policy for expression tasks.
#'
#' Purpose:
#' - Convert the user-facing `output` choice into the stored policy used by
#'   task submission.
#'
#' Parameters:
#' - `output`: `"none"` to skip result storage or `"all"` to save the full
#'   expression value.
#'
#' Returns:
#' - A list with `output` and `save_result` fields.
#'
#' Assumptions and side effects:
#' - Does not inspect or evaluate the task expression.
#'
#' @keywords internal
normalize_submit_task_output <- function(output = "none") {
  if (!is.character(output) || length(output) != 1 || is.na(output)) {
    stop("`output` must be one of: \"all\" or \"none\".")
  }

  if (!output %in% c("all", "none")) {
    stop("`output` must be one of: \"all\" or \"none\".")
  }

  list(
    output = output,
    save_result = identical(output, "all")
  )
}

#' Normalize output policy for function-call tasks.
#'
#' Purpose:
#' - Convert the user-facing `output` choice into result-saving and optional
#'   field-filtering instructions.
#'
#' Parameters:
#' - `output`: `"none"` to skip result storage, `"all"` to save the full
#'   return value, or a character vector of fields to keep from a named list.
#'
#' Returns:
#' - A list with `output`, `save_result`, and `field_filter` fields.
#'
#' Assumptions and side effects:
#' - Field names are only validated here; the actual returned object is checked
#'   after the child task runs.
#'
#' @keywords internal
normalize_submit_call_output <- function(output = "none") {
  if (!is.character(output) || length(output) < 1 || any(is.na(output)) || any(!nzchar(output))) {
    stop("`output` must be \"all\", \"none\", or a non-empty character vector.")
  }

  if (length(output) == 1 && output %in% c("all", "none")) {
    return(list(
      output = output,
      save_result = identical(output, "all"),
      field_filter = NULL
    ))
  }

  list(
    output = output,
    save_result = TRUE,
    field_filter = output
  )
}

next_task_id <- function(state) {
  as.integer(state$next_id)
}

append_pending_task <- function(state, item) {
  requested_slots <- as.integer(item$resources$slots %||% 1L)
  capacity_slots <- as.integer(state$capacity$slots %||% 1L)
  if (requested_slots > capacity_slots) {
    stop(
      "Task requests ", requested_slots, " slot(s), but queue capacity is ",
      capacity_slots, ". Reduce `resources$slots` or increase `max_slots`."
    )
  }

  state$pending[[length(state$pending) + 1L]] <- item
  state$next_id <- as.integer(state$next_id) + 1L

  state
}

build_submit_call_expr <- function(fun, args = list(), field_filter = NULL) {
  if (!is.function(fun)) {
    stop("`fun` must be a function.")
  }

  if (!is.list(args)) {
    stop("`args` must be a list.")
  }

  if (!is.null(field_filter)) {
    if (!is.character(field_filter) || length(field_filter) < 1 || any(is.na(field_filter))) {
      stop("`field_filter` must be NULL or a character vector.")
    }
  }

  bquote({
    .__result <- do.call(.(fun), .(args))

    if (!is.null(.(field_filter))) {
      if (!is.list(.__result) || is.null(names(.__result))) {
        stop("`output` field filtering requires `fun` to return a named list.")
      }

      .__missing <- setdiff(.(field_filter), names(.__result))
      if (length(.__missing) > 0) {
        stop(
          "Requested output field(s) are missing from the function result: ",
          paste(.__missing, collapse = ", ")
        )
      }

      .__result <- .__result[.(field_filter)]
    }

    .__result
  })
}

normalize_grid_rows <- function(grid) {
  if (is.data.frame(grid)) {
    if (nrow(grid) == 0) {
      return(list())
    }

    return(lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, , drop = FALSE])))
  }

  if (is.list(grid)) {
    if (length(grid) == 0) {
      return(list())
    }

    bad_idx <- which(!vapply(grid, is.list, logical(1)))[1]
    if (!is.na(bad_idx)) {
      stop("When `grid` is a list, each element must be a named list.")
    }

    bad_name_idx <- which(vapply(
      grid,
      function(x) is.null(names(x)) || any(!nzchar(names(x))),
      logical(1)
    ))[1]
    if (!is.na(bad_name_idx)) {
      stop("Each `grid` element must be a named list.")
    }

    return(grid)
  }

  stop("`grid` must be a data.frame or a list of named lists.")
}

split_map_extra_args <- function(extra_args) {
  arg_names <- names(extra_args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(extra_args))
  }

  if (any(!nzchar(arg_names))) {
    stop("All `...` arguments in `map_tasks()` must be named.")
  }

  dynamic_idx <- grepl("^fun_", arg_names)
  dynamic_args <- extra_args[dynamic_idx]
  static_args <- extra_args[!dynamic_idx]

  if (length(dynamic_args) > 0) {
    non_fun <- names(dynamic_args)[!vapply(dynamic_args, is.function, logical(1))]
    if (length(non_fun) > 0) {
      stop(
        "Dynamic argument generator(s) must be functions: ",
        paste(non_fun, collapse = ", ")
      )
    }
  }

  list(
    static = static_args,
    dynamic = dynamic_args
  )
}

run_dynamic_generators <- function(dynamic_args, vars) {
  if (length(dynamic_args) == 0) {
    return(list())
  }

  out <- list()
  for (name in names(dynamic_args)) {
    target_name <- sub("^fun_", "", name)
    generator <- dynamic_args[[name]]
    required <- names(formals(generator))

    if (is.null(required) || length(required) == 0) {
      out[[target_name]] <- generator()
      next
    }

    if ("..." %in% required) {
      required <- setdiff(required, "...")
    }

    missing_vars <- setdiff(required, names(vars))
    if (length(missing_vars) > 0) {
      stop(
        "Dynamic generator `", name, "` requires missing grid variable(s): ",
        paste(missing_vars, collapse = ", ")
      )
    }

    out[[target_name]] <- do.call(generator, vars[required])
  }

  out
}

render_label_template <- function(label_fmt = NULL, vars = list()) {
  if (is.null(label_fmt)) {
    return(NULL)
  }

  if (!is.character(label_fmt) || length(label_fmt) != 1 || is.na(label_fmt) || !nzchar(label_fmt)) {
    stop("`label_fmt` must be NULL or a single non-empty character string.")
  }

  matches <- gregexpr("\\{([^}]+)\\}", label_fmt, perl = TRUE)
  tokens <- regmatches(label_fmt, matches)[[1]]
  keys <- unique(gsub("^\\{|\\}$", "", tokens))

  missing_keys <- setdiff(keys, names(vars))
  if (length(missing_keys) > 0) {
    stop(
      "`label_fmt` references missing grid variable(s): ",
      paste(missing_keys, collapse = ", ")
    )
  }

  out <- label_fmt
  for (key in keys) {
    value <- vars[[key]]
    value_text <- paste(as.character(value), collapse = "_")
    out <- gsub(paste0("\\{", key, "\\}"), value_text, out, perl = TRUE)
  }

  out
}

build_task_item <- function(
    id,
    label,
    expr,
    priority,
    resources,
    output,
    save_result,
    import = "auto",
    context) {
  result_path <- NULL
  if (isTRUE(save_result)) {
    result_path <- task_tmpfile(id)
  }

  list(
    id = id,
    label = label,
    expr = expr,
    resources = resources,
    priority = priority,
    output = output,
    save_result = save_result,
    import = import,
    context = context,
    status = "pending",
    submit_time = Sys.time(),
    start_time = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
    end_time = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
    task = NULL,
    progress = NA_real_,
    message = NA_character_,
    stdout_buffer = "",
    stderr_buffer = "",
    error = NULL,
    result_path = result_path,
    start_task = function(task_item) {
      start_task_process(
        id = task_item$id,
        expr = task_item$expr,
        result_path = task_item$result_path,
        import = task_item$context$env,
        packages = task_item$context$packages,
        workdir = task_item$context$workdir,
        env = task_item$context$vars,
        save_result = task_item$save_result
      )
    }
  )
}

#' Submit an Expression Task to the Queue
#'
#' Purpose:
#' - Capture one expression and enqueue it for background execution.
#'
#' @param expr Expression to evaluate in a child process.
#' @param label Optional user-facing task label.
#' @param priority Integer-like priority. Larger values run earlier.
#' @param resources Resource declaration list. v0.4 uses `list(slots = 1)`.
#' @param import Task context import mode. Use `"auto"` (default), `"none"`,
#'   or `list(env = ..., packages = ..., workdir = ..., vars = ...)`.
#' @param output Either `"none"` or `"all"`. `"none"` skips result file
#'   saving and is the default. Use `"all"` when you need
#'   `get_task_result()` to return the expression value.
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_slots = 1)
#' if (interactive() && requireNamespace("callr", quietly = TRUE)) {
#'   on.exit(shutdown_queue(), add = TRUE)
#'   submit_code({ 1 + 1 }, label = "toy_expr", output = "all")
#' }
#' @export
submit_code <- function(
    expr,
    label = NULL,
    priority = 0L,
    resources = list(slots = 1L),
    import = "auto",
    output = "none") {
  if (missing(expr)) {
    stop("`expr` is required.")
  }

  ensure_queue_initialized()

  validate_label(label)
  priority <- validate_priority(priority)
  resources <- normalize_resources(resources)
  output_spec <- normalize_submit_task_output(output)

  id <- next_task_id(pkg_env$scheduler)
  expr <- substitute(expr)
  context <- resolve_submit_context(import = import, expr = expr)

  item <- build_task_item(
    id = id,
    label = label,
    expr = expr,
    priority = priority,
    resources = resources,
    output = output_spec$output,
    save_result = output_spec$save_result,
    import = import,
    context = context
  )

  pkg_env$scheduler <- append_pending_task(pkg_env$scheduler, item)
  pkg_env$scheduler <- update_queue(pkg_env$scheduler)
  if (scheduler_has_work(pkg_env$scheduler)) {
    start_scheduler()
  }
  write_dashboard_snapshot()
  maybe_auto_launch_dashboard()
  invisible(NULL)
}

#' Submit a Function Call Task to the Queue
#'
#' Purpose:
#' - Enqueue one function-call style task with explicit arguments.
#'
#' @param fun Function object to run in the child process.
#' @param args List of arguments passed to `fun` via `do.call()`.
#' @param label Optional user-facing task label.
#' @param priority Integer-like priority. Larger values run earlier.
#' @param resources Resource declaration list. v0.4 uses `list(slots = 1)`.
#' @param import Task context import mode. Use `"auto"` (default), `"none"`,
#'   or `list(env = ..., packages = ..., workdir = ..., vars = ...)`.
#' @param output `"none"`, `"all"`, or a character vector of result fields to
#'   keep. `"none"` skips result file saving and is the default.
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_slots = 1)
#' if (interactive() && requireNamespace("callr", quietly = TRUE)) {
#'   on.exit(shutdown_queue(), add = TRUE)
#'   submit_task(fun = sum, args = list(1, 2, 3), label = "toy_call", output = "all")
#' }
#' @export
submit_task <- function(
    fun,
    args = list(),
    label = NULL,
    priority = 0L,
    resources = list(slots = 1L),
    import = "auto",
    output = "none") {
  if (missing(fun)) {
    stop("`fun` is required.")
  }

  ensure_queue_initialized()

  validate_label(label)
  priority <- validate_priority(priority)
  resources <- normalize_resources(resources)
  output_spec <- normalize_submit_call_output(output)
  context <- resolve_submit_context(import = import, fun = fun)

  id <- next_task_id(pkg_env$scheduler)
  expr <- build_submit_call_expr(
    fun = fun,
    args = args,
    field_filter = output_spec$field_filter
  )

  item <- build_task_item(
    id = id,
    label = label,
    expr = expr,
    priority = priority,
    resources = resources,
    output = output_spec$output,
    save_result = output_spec$save_result,
    import = import,
    context = context
  )

  pkg_env$scheduler <- append_pending_task(pkg_env$scheduler, item)
  pkg_env$scheduler <- update_queue(pkg_env$scheduler)
  if (scheduler_has_work(pkg_env$scheduler)) {
    start_scheduler()
  }
  write_dashboard_snapshot()
  maybe_auto_launch_dashboard()
  invisible(NULL)
}

#' Submit a Grid of Function Calls to the Queue
#'
#' Purpose:
#' - Expand a parameter grid into multiple `submit_task()` submissions.
#' - Support `fun_` dynamic argument generators based on each grid row.
#'
#' @param fun Function object to run for each grid row.
#' @param grid Data frame or list of named lists. Each row/item defines one run.
#' @param fixed_args Named list of shared arguments merged into every call.
#' @param ... Additional named arguments. Names prefixed with `fun_` are treated
#'   as dynamic generators and must be functions.
#' @param label_fmt Optional label template with `{column}` placeholders.
#' @param priority Integer-like priority for all generated tasks.
#' @param resources Resource declaration list. v0.4 uses `list(slots = 1)`.
#' @param import Task context import mode. Use `"auto"` (default), `"none"`,
#'   or `list(env = ..., packages = ..., workdir = ..., vars = ...)`.
#' @param output Output policy passed to `submit_task()`. Defaults to `"none"`
#'   to avoid writing large batch outputs unless requested.
#' @return Invisibly returns `NULL`.
#' @examples
#' init_queue(max_slots = 1)
#' grid <- data.frame(model = c("a", "b"), stringsAsFactors = FALSE)
#' if (interactive() && requireNamespace("callr", quietly = TRUE)) {
#'   on.exit(shutdown_queue(), add = TRUE)
#'   map_tasks(
#'     fun = function(formula) formula,
#'     grid = grid,
#'     fun_formula = function(model) paste0("y ~ ", model),
#'     label_fmt = "fit_{model}"
#'   )
#' }
#' @export
map_tasks <- function(
    fun,
    grid,
    fixed_args = list(),
    ...,
    label_fmt = NULL,
    priority = 0L,
    resources = list(slots = 1L),
    import = "auto",
    output = "none") {
  if (missing(fun)) {
    stop("`fun` is required.")
  }
  if (!is.function(fun)) {
    stop("`fun` must be a function.")
  }
  if (!is.list(fixed_args)) {
    stop("`fixed_args` must be a list.")
  }

  ensure_queue_initialized()

  rows <- normalize_grid_rows(grid)
  if (length(rows) == 0) {
    return(invisible(NULL))
  }

  extras <- split_map_extra_args(list(...))
  static_args <- extras$static
  dynamic_args <- extras$dynamic

  for (vars in rows) {
    run_args <- utils::modifyList(fixed_args, static_args)
    dynamic_values <- run_dynamic_generators(dynamic_args, vars)
    run_args <- utils::modifyList(run_args, dynamic_values)
    label <- render_label_template(label_fmt = label_fmt, vars = vars)

    submit_task(
      fun = fun,
      args = run_args,
      label = label,
      priority = priority,
      resources = resources,
      import = import,
      output = output
    )
  }

  invisible(NULL)
}
