# Shiny Queue Dashboard (User-Facing)
#
# Purpose:
# - Provide a visual dashboard for queue monitoring and light task control.
# - Keep the workflow simple for research users who prefer visual feedback
#   over repeated console polling.

# Render one horizontal progress bar.
# Args:
# - ratio: Numeric in [0, 1].
# - label: Optional label shown above the bar.
# Returns:
# - A `shiny::div` tag.
progress_bar_tag <- function(ratio = 0, label = NULL) {
  pct <- max(0, min(1, as.numeric(ratio %||% 0))) * 100

  shiny::div(
    class = "metric-progress-wrap",
    if (!is.null(label)) shiny::div(class = "metric-progress-label", label),
    shiny::div(
      class = "metric-progress-track",
      shiny::div(class = "metric-progress-fill", style = sprintf("width: %.1f%%;", pct))
    )
  )
}

task_expand_block_ui <- function(task, expanded = FALSE) {
  if (!isTRUE(expanded)) {
    return(NULL)
  }

  log_text <- dashboard_log_text_from_row(task, tail_n = 120L)

  shiny::div(
    class = "task-expand-block",
    shiny::div(class = "task-expand-meta", paste("Status:", status_display_label(task$status))),
    shiny::div(class = "task-expand-meta", paste("Priority:", task$priority)),
    shiny::div(class = "task-expand-meta", paste("Submitted:", task$submit_time_label)),
    shiny::div(class = "task-expand-meta", paste("Started:", task$start_time_label)),
    shiny::div(class = "task-expand-meta", paste("Ended:", task$end_time_label)),
    shiny::tags$pre(
      class = "task-expand-logs",
      `data-scroll-key` = sprintf("logs-%s", task$id),
      log_text
    )
  )
}

running_task_card_ui <- function(task, expanded = FALSE, allow_cancel = TRUE, chain_tab = NULL, progress_ratio = NULL) {
  select_id <- button_id_for_task("select", task$id)
  cancel_id <- button_id_for_task("cancel", task$id)
  if (is.null(progress_ratio) || length(progress_ratio) == 0 || is.na(progress_ratio)) {
    progress_ratio <- normalize_progress_fraction(task$progress, default_fraction = 0.5)
  } else {
    progress_ratio <- normalize_progress_fraction(progress_ratio, default_fraction = 0.5)
  }
  if (is.null(chain_tab)) {
    chain_tab <- dashboard_stan_chain_progress_from_row(task)
  }
  msg <- as.character(task$message %||% "")
  show_msg <- !is.na(msg) && nzchar(trimws(msg))
  start_epoch <- if (is.na(task$start_time)) NA_real_ else as.numeric(as.POSIXct(task$start_time))
  start_epoch_attr <- if (is.na(start_epoch)) "" else sprintf("%.6f", start_epoch)

  shiny::div(
    class = dashboard_card_class(task$status),
    `data-task-id` = as.character(task$id),
    shiny::div(
      class = "task-card-head",
      shiny::div(class = "task-card-title", task$label),
      shiny::span(class = paste("status-pill", status_badge_class(task$status)), status_display_label(task$status))
    ),
    shiny::div(class = "task-card-meta", paste("ID:", task$id)),
    shiny::div(
      class = "task-card-meta",
      shiny::span("Running: "),
      shiny::span(
        class = "js-running-elapsed",
        `data-start-epoch` = start_epoch_attr,
        task$running_elapsed
      )
    ),
    shiny::div(class = "task-card-meta", paste("Started:", task$start_time_label)),
    if (isTRUE(show_msg)) shiny::div(class = "task-card-msg", msg),
    if (nrow(chain_tab) > 0) {
      shiny::div(
        class = "chain-block",
        lapply(seq_len(nrow(chain_tab)), function(i) {
          chain <- chain_tab$chain[[i]]
          phase <- chain_tab$phase[[i]]
          p <- max(0, min(1, as.numeric(chain_tab$progress[[i]])))
          label <- if (!is.na(phase) && nzchar(phase)) {
            sprintf("Chain %d (%s)", chain, phase)
          } else {
            sprintf("Chain %d", chain)
          }

          shiny::div(
            class = "chain-row",
            shiny::div(class = "chain-label", label),
            shiny::div(
              class = "chain-track",
              shiny::div(class = "chain-fill", style = sprintf("width: %.1f%%;", 100 * p))
            )
          )
        })
      )
    } else {
      shiny::div(
        class = "task-progress-track",
        shiny::div(
          class = "task-progress-fill",
          style = sprintf("width: %.1f%%;", 100 * progress_ratio)
        )
      )
    },
    shiny::div(
      class = "task-card-actions",
      shiny::actionButton(select_id, ifelse(expanded, "Collapse", "Details"), class = "btn btn-sm btn-outline-primary"),
      if (isTRUE(allow_cancel)) {
        shiny::actionButton(cancel_id, "Cancel", class = "btn btn-sm btn-outline-danger")
      }
    ),
    task_expand_block_ui(task, expanded = expanded)
  )
}

# Render one queued-task card with wait and priority info.
# Args:
# - task: One-row data.frame for a queued task.
# Returns:
# - A `shiny::div` card tag.
queued_task_card_ui <- function(task, expanded = FALSE, allow_cancel = TRUE) {
  select_id <- button_id_for_task("select", task$id)
  cancel_id <- button_id_for_task("cancel", task$id)

  shiny::div(
    class = dashboard_card_class(task$status),
    `data-task-id` = as.character(task$id),
    shiny::div(
      class = "task-card-head",
      shiny::div(class = "task-card-title", task$label),
      shiny::span(class = paste("status-pill", status_badge_class(task$status)), status_display_label(task$status))
    ),
    shiny::div(class = "task-card-meta", paste("ID:", task$id)),
    shiny::div(class = "task-card-meta", paste("Priority:", task$priority)),
    shiny::div(class = "task-card-meta", paste("Waiting:", task$queue_wait)),
    shiny::div(class = "task-card-meta", paste("Submitted:", task$submit_time_label)),
    shiny::div(
      class = "task-card-actions",
      shiny::actionButton(select_id, ifelse(expanded, "Collapse", "Details"), class = "btn btn-sm btn-outline-primary"),
      if (isTRUE(allow_cancel)) {
        shiny::actionButton(cancel_id, "Cancel", class = "btn btn-sm btn-outline-danger")
      }
    ),
    task_expand_block_ui(task, expanded = expanded)
  )
}

# Render one terminal-task card (completed/failed/cancelled).
# Args:
# - task: One-row data.frame for a terminal task.
# Returns:
# - A `shiny::div` card tag.
finished_task_card_ui <- function(task, expanded = FALSE) {
  select_id <- button_id_for_task("select", task$id)

  shiny::div(
    class = dashboard_card_class(task$status),
    `data-task-id` = as.character(task$id),
    shiny::div(
      class = "task-card-head",
      shiny::div(class = "task-card-title", task$label),
      shiny::span(class = paste("status-pill", status_badge_class(task$status)), status_display_label(task$status))
    ),
    shiny::div(class = "task-card-meta", paste("ID:", task$id)),
    shiny::div(class = "task-card-meta", paste("Ended:", task$end_time_label)),
    if (identical(task$status, "failed") && nzchar(task$error %||% "")) {
      shiny::div(class = "task-card-error", task$error)
    },
    shiny::div(
      class = "task-card-actions",
      shiny::actionButton(select_id, ifelse(expanded, "Collapse", "Details"), class = "btn btn-sm btn-outline-primary")
    ),
    task_expand_block_ui(task, expanded = expanded)
  )
}

# Render one dashboard column section from task rows.
# Args:
# - tab: Subset table for one status block.
# - title: Section title.
# - renderer: Card renderer function for each row.
# Returns:
# - A section wrapper tag.
column_cards_ui <- function(tab, title, renderer, header_right = NULL, subheader = NULL) {
  cards <- if (nrow(tab) == 0) {
    shiny::div(class = "empty-block", "No tasks")
  } else {
    lapply(seq_len(nrow(tab)), function(i) renderer(tab[i, , drop = FALSE]))
  }

  shiny::div(
    class = "dashboard-column",
    shiny::div(
      class = "dashboard-column-header",
      shiny::div(class = "dashboard-column-title", sprintf("%s (%d)", title, nrow(tab))),
      header_right
    ),
    subheader,
    shiny::div(
      class = "dashboard-column-body",
      `data-scroll-key` = sprintf("col-%s", tolower(title)),
      cards
    )
  )
}

finished_filter_controls_ui <- function(completed_n, failed_n, cancelled_n, selected = "completed") {
  btn_class <- function(key) {
    base <- "btn btn-sm finished-filter-btn"
    if (identical(selected, key)) {
      return(paste(base, "active"))
    }
    base
  }

  shiny::div(
    class = "finished-filter-row",
    shiny::actionButton("finished_filter_completed", sprintf("completed (%d)", completed_n), class = btn_class("completed")),
    shiny::actionButton("finished_filter_failed", sprintf("failed (%d)", failed_n), class = btn_class("failed")),
    shiny::actionButton("finished_filter_cancelled", sprintf("cancelled (%d)", cancelled_n), class = btn_class("cancelled"))
  )
}

# Provide CSS used by the queue dashboard layout.
# Returns:
# - One `<style>` tag.
dashboard_css <- function() {
  shiny::tags$style(shiny::HTML(
    paste(
      ".dashboard-root { padding: 12px; background: #f6f8fb; }",
      ".dashboard-row { margin-bottom: 12px; }",
      ".summary-card, .block-card, .detail-card { background: #fff; border-radius: 10px; border: 1px solid #dbe2ea; padding: 12px; }",
      ".metric-progress-wrap { margin-bottom: 8px; }",
      ".metric-progress-label { font-size: 12px; margin-bottom: 4px; color: #3d4a58; }",
      ".metric-progress-track { width: 100%; height: 10px; background: #e6edf5; border-radius: 999px; overflow: hidden; }",
      ".metric-progress-fill { height: 100%; background: linear-gradient(90deg, #2d6cdf, #57a7ff); }",
      ".dashboard-column-title { font-size: 15px; font-weight: 700; margin-bottom: 8px; color: #22384d; }",
      ".dashboard-column-header { display: flex; justify-content: space-between; align-items: center; gap: 10px; margin-bottom: 8px; }",
      ".dashboard-column-title { margin-bottom: 0; }",
      ".dashboard-column-body { display: flex; flex-direction: column; gap: 8px; max-height: 35vh; overflow-y: auto; }",
      ".dashboard-column-body { align-items: flex-start; }",
      ".task-card { border: 1px solid #d7e0e8; border-radius: 8px; padding: 9px; background: #fbfdff; }",
      ".task-card { display: flex; flex-direction: column; gap: 4px; }",
      ".task-card { width: min(100%, 980px); }",
      ".task-card-failed { border-color: #d9534f; background: #fff4f4; }",
      ".task-card-head { display: flex; justify-content: space-between; align-items: center; gap: 8px; margin-bottom: 5px; }",
      ".task-card-title { font-weight: 600; color: #17314a; word-break: break-word; }",
      ".task-card-meta { font-size: 12px; color: #4a5b6d; margin-bottom: 2px; }",
      ".task-card-msg { font-size: 12px; color: #253749; margin-top: 4px; margin-bottom: 6px; word-break: break-word; }",
      ".task-card-error { margin-top: 6px; font-size: 12px; color: #9f1d1d; white-space: pre-wrap; word-break: break-word; }",
      ".task-card-actions { margin-top: 8px; display: flex; gap: 6px; }",
      ".task-card-actions { margin-top: auto; }",
      ".task-expand-block { margin-top: 8px; padding-top: 8px; border-top: 1px dashed #d7e0e8; }",
      ".task-expand-meta { font-size: 12px; color: #334b63; margin-bottom: 2px; }",
      ".task-expand-logs { margin-top: 8px; background: #0f1720; color: #d7e2ef; border-radius: 6px; border: 1px solid #29384a; padding: 8px; font-size: 11px; line-height: 1.35; white-space: pre-wrap; word-break: break-word; max-height: 220px; overflow-y: auto; }",
      ".task-progress-track { width: 100%; height: 8px; background: #e7edf3; border-radius: 999px; overflow: hidden; margin-top: 4px; }",
      ".task-progress-fill { height: 100%; background: linear-gradient(90deg, #1f7a5b, #31b37f); }",
      ".chain-block { margin-top: 4px; display: flex; flex-direction: column; gap: 5px; }",
      ".chain-row { display: grid; grid-template-columns: 140px 1fr; align-items: center; gap: 8px; }",
      ".chain-label { font-size: 12px; color: #3a4d61; }",
      ".chain-track { width: 100%; height: 8px; background: #e7edf3; border-radius: 999px; overflow: hidden; }",
      ".chain-fill { height: 100%; background: linear-gradient(90deg, #1967a3, #4eb2ff); }",
      ".status-pill { font-size: 11px; border-radius: 999px; padding: 2px 8px; border: 1px solid transparent; text-transform: lowercase; }",
      ".status-running { background: #ebf8f1; color: #0d6f4b; border-color: #9cd8bd; }",
      ".status-queued { background: #f2f5f8; color: #475867; border-color: #cfd9e3; }",
      ".status-completed { background: #edf6ff; color: #1f5f99; border-color: #b9d7f4; }",
      ".status-failed { background: #fdecec; color: #9e1c1c; border-color: #efb5b5; }",
      ".status-cancelled { background: #fff3e5; color: #9a5a00; border-color: #f2d2a3; }",
      ".finished-filter-row { display: flex; gap: 8px; margin-bottom: 10px; flex-wrap: wrap; }",
      ".finished-filter-btn { border: 1px solid #cfd8e2; background: #f7f9fc; color: #304558; }",
      ".finished-filter-btn.active { background: #2d6cdf; color: #fff; border-color: #2d6cdf; }",
      ".empty-block { color: #667788; font-style: italic; padding: 6px 0; }",
      ".shiny-text-output { margin-bottom: 0; }",
      sep = "\n"
    )
  ))
}

# Provide JavaScript used to preserve scroll positions during UI re-renders.
# Returns:
# - One `<script>` tag.
dashboard_scroll_js <- function() {
  shiny::tags$script(shiny::HTML(
    paste(
      "(function () {",
      "  if (window.__taskrScrollStore) return;",
      "  window.__taskrScrollStore = {};",
      "  function keyOf(el) { return el && el.getAttribute ? el.getAttribute('data-scroll-key') : null; }",
      "  function save(el) {",
      "    var key = keyOf(el);",
      "    if (!key) return;",
      "    window.__taskrScrollStore[key] = el.scrollTop || 0;",
      "  }",
      "  function restore(scope) {",
      "    var root = scope || document;",
      "    var nodes = root.querySelectorAll ? root.querySelectorAll('[data-scroll-key]') : [];",
      "    Array.prototype.forEach.call(nodes, function (el) {",
      "      var key = keyOf(el);",
      "      if (!key) return;",
      "      if (Object.prototype.hasOwnProperty.call(window.__taskrScrollStore, key)) {",
      "        el.scrollTop = window.__taskrScrollStore[key];",
      "      }",
      "    });",
      "  }",
      "  document.addEventListener('scroll', function (evt) {",
      "    var el = evt.target;",
      "    if (!el || !el.getAttribute) return;",
      "    if (el.getAttribute('data-scroll-key')) save(el);",
      "  }, true);",
      "  document.addEventListener('shiny:value', function (evt) {",
      "    setTimeout(function () { restore(evt && evt.target ? evt.target : document); }, 0);",
      "  });",
      "  document.addEventListener('shiny:connected', function () { restore(document); });",
      "  function formatDuration(sec) {",
      "    if (!isFinite(sec) || sec < 0) sec = 0;",
      "    sec = Math.floor(sec);",
      "    var h = Math.floor(sec / 3600);",
      "    var m = Math.floor((sec % 3600) / 60);",
      "    var s = sec % 60;",
      "    if (h > 0) {",
      "      return h + 'h ' + String(m).padStart(2, '0') + 'm ' + String(s).padStart(2, '0') + 's';",
      "    }",
      "    if (m > 0) {",
      "      return m + 'm ' + String(s).padStart(2, '0') + 's';",
      "    }",
      "    return s + 's';",
      "  }",
      "  function updateRunningElapsed(root) {",
      "    var scope = root || document;",
      "    var nodes = scope.querySelectorAll ? scope.querySelectorAll('.js-running-elapsed[data-start-epoch]') : [];",
      "    var nowSec = Date.now() / 1000;",
      "    Array.prototype.forEach.call(nodes, function (el) {",
      "      var raw = el.getAttribute('data-start-epoch');",
      "      var startSec = parseFloat(raw);",
      "      if (!isFinite(startSec) || !raw) return;",
      "      el.textContent = formatDuration(nowSec - startSec);",
      "    });",
      "  }",
      "  setInterval(function () { updateRunningElapsed(document); }, 1000);",
      "  document.addEventListener('shiny:value', function (evt) {",
      "    setTimeout(function () { updateRunningElapsed(evt && evt.target ? evt.target : document); }, 0);",
      "  });",
      "  document.addEventListener('shiny:connected', function () { updateRunningElapsed(document); });",
      "  if (window.Shiny && window.Shiny.addCustomMessageHandler) {",
      "    window.Shiny.addCustomMessageHandler('taskr_focus_task', function (msg) {",
      "      var taskId = msg && msg.task_id ? String(msg.task_id) : '';",
      "      if (!taskId) return;",
      "      var cards = document.querySelectorAll ? document.querySelectorAll('.task-card[data-task-id]') : [];",
      "      var card = null;",
      "      Array.prototype.forEach.call(cards, function (el) {",
      "        if (card) return;",
      "        if (String(el.getAttribute('data-task-id') || '') === taskId) card = el;",
      "      });",
      "      if (!card) return;",
      "      var key = msg && msg.container_key ? String(msg.container_key) : '';",
      "      var container = null;",
      "      if (key && document.querySelector) {",
      "        container = document.querySelector('[data-scroll-key=\"' + key + '\"]');",
      "      }",
      "      if (!container && card.closest) container = card.closest('.dashboard-column-body');",
      "      if (!container) return;",
      "      var top = card.offsetTop;",
      "      var bottom = top + card.offsetHeight;",
      "      var viewTop = container.scrollTop;",
      "      var viewBottom = viewTop + container.clientHeight;",
      "      var margin = 8;",
      "      if (top < (viewTop + margin) || bottom > (viewBottom - margin)) {",
      "        container.scrollTop = Math.max(0, top - 12);",
      "      }",
      "    });",
      "  }",
      "})();",
      sep = "\n"
    )
  ))
}

# Build the static dashboard UI shell.
# Returns:
# - A `shiny::fluidPage` object.
queue_dashboard_ui <- function() {
  shiny::fluidPage(
    dashboard_css(),
    dashboard_scroll_js(),
    shiny::div(
      class = "dashboard-root",
      shiny::fluidRow(
        class = "dashboard-row",
        shiny::column(
          width = 12,
            shiny::div(
              class = "summary-card",
              shiny::div(
                style = "display:flex; justify-content:space-between; align-items:center; gap:12px; margin-bottom:8px;",
                shiny::h3("Task Monitor", style = "margin:0;"),
                shiny::uiOutput("summary_actions")
              ),
              shiny::textInput("task_query", "Search", value = "", placeholder = "label or task id"),
              shiny::uiOutput("summary_progress")
            )
          )
      ),
      shiny::fluidRow(
        class = "dashboard-row",
        shiny::column(
          width = 12,
          shiny::div(
            class = "block-card",
            shiny::uiOutput("running_cards")
          )
        )
      ),
      shiny::fluidRow(
        class = "dashboard-row",
        shiny::column(width = 12, shiny::div(class = "block-card", shiny::uiOutput("queued_cards")))
      ),
      shiny::fluidRow(
        class = "dashboard-row",
        shiny::column(width = 12, shiny::div(class = "block-card", shiny::uiOutput("finished_cards")))
      )
    )
  )
}

queue_dashboard_app <- function(data_mode = c("live", "snapshot"), snapshot_path = NULL, command_path = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("`launch_dashboard()` requires the `shiny` package. Install it with install.packages('shiny').")
  }

  data_mode <- match.arg(data_mode)
  read_only <- identical(data_mode, "snapshot")
  control_via_commands <- isTRUE(read_only) &&
    !is.null(command_path) &&
    is.character(command_path) &&
    length(command_path) == 1 &&
    !is.na(command_path) &&
    nzchar(command_path)
  can_control <- !isTRUE(read_only) || isTRUE(control_via_commands)
  if (!isTRUE(read_only)) {
    ensure_queue_initialized()
  }

  app <- shiny::shinyApp(
    ui = queue_dashboard_ui(),
    server = function(input, output, session) {
      selected_id <- shiny::reactiveVal(NULL)
      pending_running_cancel <- shiny::reactiveVal(NULL)
      refresh_nonce <- shiny::reactiveVal(0L)
      observed_select_ids <- shiny::reactiveVal(character())
      observed_cancel_ids <- shiny::reactiveVal(character())
      finished_filter_status <- shiny::reactiveVal("completed")
      display_progress_cache <- shiny::reactiveVal(list())
      pending_cancel_ids <- shiny::reactiveVal(character())
      action_cooldown <- shiny::reactiveVal(list())
      read_snapshot_tasks <- function() {
        out <- tryCatch(read_dashboard_snapshot(path = snapshot_path), error = function(e) NULL)
        if (is.null(out)) {
          return(list(tasks = empty_dashboard_table(), max_slots = 1L))
        }
        list(
          tasks = out$tasks %||% empty_dashboard_table(),
          max_slots = as.integer(out$max_slots %||% 1L)
        )
      }

      state_tasks <- shiny::reactivePoll(
        intervalMillis = 1000,
        session = session,
        checkFunc = function() {
          tab <- if (isTRUE(read_only)) {
            read_snapshot_tasks()$tasks
          } else {
            tryCatch(extract_dashboard_snapshot(now = Sys.time()), error = function(e) empty_dashboard_table())
          }
          paste0(dashboard_state_signature(tab), "-", refresh_nonce())
        },
        valueFunc = function() {
          tab <- if (isTRUE(read_only)) {
            read_snapshot_tasks()$tasks
          } else {
            tryCatch(extract_dashboard_snapshot(now = Sys.time()), error = function(e) empty_dashboard_table())
          }
          add_dashboard_derived_columns(tab, now = Sys.time())
        }
      )

      running_tasks <- shiny::reactivePoll(
        intervalMillis = 1000,
        session = session,
        checkFunc = function() {
          tab <- if (isTRUE(read_only)) {
            read_snapshot_tasks()$tasks
          } else {
            tryCatch(extract_dashboard_snapshot(now = Sys.time()), error = function(e) empty_dashboard_table())
          }
          paste0(dashboard_running_signature(tab), "-", refresh_nonce())
        },
        valueFunc = function() {
          tab <- if (isTRUE(read_only)) {
            read_snapshot_tasks()$tasks
          } else {
            tryCatch(extract_dashboard_snapshot(now = Sys.time()), error = function(e) empty_dashboard_table())
          }
          add_dashboard_derived_columns(tab, now = Sys.time())
        }
      )

      filtered_state_tasks <- shiny::reactive({
        filter_dashboard_tasks(state_tasks(), query = input$task_query %||% "")
      })

      filtered_running_tasks <- shiny::reactive({
        tab <- running_tasks()
        tab <- tab[tab$status == "running", , drop = FALSE]
        filter_dashboard_tasks(tab, query = input$task_query %||% "")
      })

      state_split <- shiny::reactive({
        split_dashboard_tasks(filtered_state_tasks())
      })

      output$summary_progress <- shiny::renderUI({
        slots <- if (isTRUE(read_only)) {
          read_snapshot_tasks()$max_slots %||% 1L
        } else {
          pkg_env$scheduler$capacity$slots %||% 1L
        }
        summary <- dashboard_summary_metrics(filtered_state_tasks(), max_slots = slots)
        shiny::tagList(
          progress_bar_tag(
            ratio = summary$slot_ratio,
            label = sprintf("Slot usage: %d / %d", summary$slots_used, summary$slots_total)
          ),
          progress_bar_tag(
            ratio = summary$completion_ratio,
            label = sprintf("Completion: %d / %d terminal", summary$completed + summary$failed + summary$cancelled, summary$total)
          )
        )
      })

      output$summary_actions <- shiny::renderUI({
        if (!isTRUE(can_control)) {
          return(NULL)
        }

        shiny::actionButton("clear_all_tasks", "Clear All", class = "btn btn-danger btn-sm")
      })

      output$running_cards <- shiny::renderUI({
        expanded_task_id <- selected_id()
        running_tab <- filtered_running_tasks()
        now_ts <- Sys.time()
        fallback_wait_sec <- dashboard_initial_progress_wait_sec()
        progress_cache <- display_progress_cache()
        if (is.null(progress_cache)) {
          progress_cache <- list()
        }

        chain_by_id <- list()
        progress_by_id <- list()

        for (i in seq_len(nrow(running_tab))) {
          task_row <- running_tab[i, , drop = FALSE]
          task_id <- as.character(task_row$id[[1]])
          task_progress <- as.numeric(task_row$progress[[1]])
          cached_progress <- suppressWarnings(as.numeric(progress_cache[[task_id]] %||% NA_real_))

          # Parse progress from the already-polled row snapshot to avoid an
          # extra per-task log query during each UI refresh.
          parsed <- dashboard_parse_task_progress_from_row(task_row)
          chain_by_id[[task_id]] <- parsed$chain

          resolved <- resolve_dashboard_progress_fraction(
            parsed_fraction = parsed$fraction,
            task_fraction = task_progress,
            cached_fraction = cached_progress,
            start_time = task_row$start_time[[1]],
            now = now_ts,
            default_fraction = 0.5,
            fallback_after_sec = fallback_wait_sec
          )

          progress_by_id[[task_id]] <- resolved
          progress_cache[[task_id]] <- resolved
        }

        running_ids <- as.character(running_tab$id %||% character())
        if (length(progress_cache) > 0) {
          drop_ids <- setdiff(names(progress_cache), running_ids)
          if (length(drop_ids) > 0) {
            progress_cache[drop_ids] <- NULL
          }
        }
        display_progress_cache(progress_cache)

        column_cards_ui(
          running_tab,
          title = "Running",
          renderer = function(task) {
            task_id <- as.character(task$id[[1]])
            running_task_card_ui(
              {
                task_id <- as.character(task$id[[1]])
                if (task_id %in% pending_cancel_ids() && task$status[[1]] %in% c("running", "queued")) {
                  task$status[[1]] <- "canceling"
                }
                task
              },
              expanded = identical(task$id, expanded_task_id),
              allow_cancel = isTRUE(can_control),
              chain_tab = chain_by_id[[task_id]],
              progress_ratio = progress_by_id[[task_id]]
            )
          }
        )
      })

      output$queued_cards <- shiny::renderUI({
        expanded_task_id <- selected_id()
        column_cards_ui(
          state_split()$queued,
          title = "Queued",
          renderer = function(task) {
            queued_task_card_ui(
              {
                task_id <- as.character(task$id[[1]])
                if (task_id %in% pending_cancel_ids() && task$status[[1]] %in% c("running", "queued")) {
                  task$status[[1]] <- "canceling"
                }
                task
              },
              expanded = identical(task$id, expanded_task_id),
              allow_cancel = isTRUE(can_control)
            )
          }
        )
      })

      output$finished_cards <- shiny::renderUI({
        expanded_task_id <- selected_id()
        finished_all <- state_split()$finished
        completed_n <- sum(finished_all$status == "completed")
        failed_n <- sum(finished_all$status == "failed")
        cancelled_n <- sum(finished_all$status == "cancelled")
        selected_filter <- finished_filter_status()
        finished_filtered <- finished_all[finished_all$status == selected_filter, , drop = FALSE]

        column_cards_ui(
          finished_filtered,
          title = "Finished",
          renderer = function(task) finished_task_card_ui(task, expanded = identical(task$id, expanded_task_id)),
          header_right = if (isTRUE(can_control)) {
            shiny::actionButton("clean_finished", "Clean Finished", class = "btn btn-warning btn-sm")
          },
          subheader = finished_filter_controls_ui(
            completed_n = completed_n,
            failed_n = failed_n,
            cancelled_n = cancelled_n,
            selected = selected_filter
          )
        )
      })

      shiny::observeEvent(input$finished_filter_completed, {
        finished_filter_status("completed")
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$finished_filter_failed, {
        finished_filter_status("failed")
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$finished_filter_cancelled, {
        finished_filter_status("cancelled")
      }, ignoreInit = TRUE)

      can_issue_action <- function(key, cooldown_sec = 5) {
        now_num <- as.numeric(Sys.time())
        tab <- action_cooldown()
        last_num <- suppressWarnings(as.numeric(tab[[key]] %||% NA_real_))
        if (!is.na(last_num) && is.finite(last_num) && (now_num - last_num) < as.numeric(cooldown_sec)) {
          return(FALSE)
        }
        tab[[key]] <- now_num
        action_cooldown(tab)
        TRUE
      }

      observe_register_buttons <- function(prefix, ids, ids_store, handler) {
        known <- ids_store()
        new_ids <- setdiff(ids, known)

        if (length(new_ids) == 0) {
          return(invisible(NULL))
        }

        for (task_id in new_ids) {
          button_id <- button_id_for_task(prefix, task_id)
          local({
            current_task_id <- task_id
            current_button_id <- button_id
            shiny::observeEvent(input[[current_button_id]], {
              handler(current_task_id)
            }, ignoreInit = TRUE)
          })
        }

        ids_store(unique(c(known, new_ids)))
        invisible(NULL)
      }

      observe_task_buttons <- shiny::observe({
        ids <- unique(c(
          filtered_running_tasks()$id,
          state_split()$queued$id,
          state_split()$finished$id
        ))

        observe_register_buttons(
          prefix = "select",
          ids = ids,
          ids_store = observed_select_ids,
          handler = function(task_id) {
            if (identical(selected_id(), task_id)) {
              selected_id(NULL)
            } else {
              selected_id(task_id)
            }
          }
        )

        observe_register_buttons(
          prefix = "cancel",
          ids = if (isTRUE(can_control)) ids else character(),
          ids_store = observed_cancel_ids,
          handler = function(task_id) {
            current <- state_tasks()
            if (nrow(current) == 0) {
              current <- running_tasks()
            }
            row <- current[current$id == task_id, , drop = FALSE]
            if (nrow(row) == 0) {
              return(invisible(NULL))
            }

            if (identical(row$status[[1]], "running")) {
              pending_running_cancel(task_id)
              shiny::showModal(shiny::modalDialog(
                title = "Cancel running task?",
                sprintf("Task '%s' is running. Cancel now?", row$label[[1]]),
                footer = shiny::tagList(
                  shiny::modalButton("Keep running"),
                  shiny::actionButton("confirm_running_cancel", "Cancel task", class = "btn btn-danger")
                )
              ))
              return(invisible(NULL))
            }

            if (identical(row$status[[1]], "queued")) {
              if (!can_issue_action(paste0("cancel::", task_id))) {
                return(invisible(NULL))
              }

              if (isTRUE(control_via_commands)) {
                try(dashboard_enqueue_command("cancel_task", task_id = task_id, path = command_path), silent = TRUE)
                pending_cancel_ids(unique(c(pending_cancel_ids(), task_id)))
              } else {
                tryCatch(
                  {
                    cancel_task(task_id)
                    shiny::showNotification("Queued task canceled.", type = "message")
                  },
                  error = function(e) {
                    shiny::showNotification(conditionMessage(e), type = "error")
                  }
                )
              }
            }
            invisible(NULL)
          }
        )

        invisible(NULL)
      })

      shiny::observeEvent(input$confirm_running_cancel, {
        if (!isTRUE(can_control)) {
          return(invisible(NULL))
        }
        task_id <- pending_running_cancel()
        shiny::removeModal()
        if (is.null(task_id)) {
          return(invisible(NULL))
        }

        if (!can_issue_action(paste0("cancel::", task_id))) {
          pending_running_cancel(NULL)
          return(invisible(NULL))
        }

        if (isTRUE(control_via_commands)) {
          try(dashboard_enqueue_command("cancel_task", task_id = task_id, path = command_path), silent = TRUE)
          pending_cancel_ids(unique(c(pending_cancel_ids(), task_id)))
        } else {
          tryCatch(
            {
              cancel_task(task_id)
              shiny::showNotification("Running task canceled.", type = "message")
            },
            error = function(e) {
              shiny::showNotification(conditionMessage(e), type = "error")
            }
          )
        }
        pending_running_cancel(NULL)
        invisible(NULL)
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$clean_finished, {
        if (!isTRUE(can_control)) {
          return(invisible(NULL))
        }
        tryCatch(
          {
            clean_tasks()
            shiny::showNotification("Finished tasks cleaned.", type = "message")
          },
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error")
          }
        )
        refresh_nonce(refresh_nonce() + 1L)
        invisible(NULL)
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$clear_all_tasks, {
        if (!isTRUE(can_control)) {
          return(invisible(NULL))
        }

        shiny::showModal(shiny::modalDialog(
          title = "Clear all tasks?",
          "This will cancel all running and queued tasks, and remove finished task records.",
          footer = shiny::tagList(
            shiny::modalButton("Keep tasks"),
            shiny::actionButton("confirm_clear_all_tasks", "Clear all", class = "btn btn-danger")
          )
        ))
        invisible(NULL)
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$confirm_clear_all_tasks, {
        if (!isTRUE(can_control)) {
          return(invisible(NULL))
        }

        shiny::removeModal()
        if (!can_issue_action("clear_all")) {
          return(invisible(NULL))
        }

        if (isTRUE(control_via_commands)) {
          all_tab <- state_tasks()
          active_ids <- all_tab$id[all_tab$status %in% c("running", "queued")]
          if (length(active_ids) > 0) {
            pending_cancel_ids(unique(c(pending_cancel_ids(), active_ids)))
          }
          try(dashboard_enqueue_command("clear_all", path = command_path), silent = TRUE)
        } else {
          slots <- as.integer(pkg_env$scheduler$capacity$slots %||% 1L)
          if (is.na(slots) || slots < 1L) {
            slots <- 1L
          }

          tryCatch(
            {
              shutdown_queue()
              init_queue(max_slots = slots)
              display_progress_cache(list())
              selected_id(NULL)
              shiny::showNotification("All tasks cleared.", type = "message")
            },
            error = function(e) {
              shiny::showNotification(conditionMessage(e), type = "error")
            }
          )
        }

        refresh_nonce(refresh_nonce() + 1L)
        invisible(NULL)
      }, ignoreInit = TRUE)

      shiny::observe({
        all_tab <- state_tasks()
        active_ids <- all_tab$id[all_tab$status %in% c("running", "queued")]
        pending_cancel_ids(intersect(pending_cancel_ids(), active_ids))
      })

      shiny::observe({
        # Keep expanded id valid when data updates.
        current_id <- selected_id()
        if (is.null(current_id)) return(invisible(NULL))

        visible_ids <- unique(c(
          filtered_running_tasks()$id,
          state_split()$queued$id,
          state_split()$finished$id
        ))
        if (length(visible_ids) == 0) return(invisible(NULL))
        if (!current_id %in% visible_ids) {
          selected_id(NULL)
        }
      })

      shiny::observe({
        current_id <- selected_id()
        if (is.null(current_id)) {
          return(invisible(NULL))
        }

        running_ids <- filtered_running_tasks()$id
        queued_ids <- state_split()$queued$id
        finished_all <- state_split()$finished
        selected_filter <- finished_filter_status()
        finished_filtered_ids <- finished_all$id[finished_all$status == selected_filter]

        container_key <- NULL
        if (current_id %in% running_ids) {
          container_key <- "col-running"
        } else if (current_id %in% queued_ids) {
          container_key <- "col-queued"
        } else if (current_id %in% finished_filtered_ids) {
          container_key <- "col-finished"
        }

        if (is.null(container_key)) {
          return(invisible(NULL))
        }

        session$onFlushed(function() {
          session$sendCustomMessage(
            "taskr_focus_task",
            list(task_id = current_id, container_key = container_key)
          )
        }, once = TRUE)
      })

      shiny::onSessionEnded(function() {
        observe_task_buttons$destroy()
      })
    }
  )

  app
}

#' Open the Shiny Queue Dashboard
#'
#' Purpose:
#' - Open a visual monitoring dashboard for queued/running/terminal tasks.
#' - Provide lightweight control actions (`cancel_task`, `clean_tasks`) without
#'   changing the existing queue API.
#'
#' @param open_viewer Whether to open dashboard URL in IDE Viewer when
#'   available.
#' @return Invisibly returns the dashboard URL.
#' @examples
#' \dontrun{
#' init_queue(max_slots = 2)
#' launch_dashboard()
#' }
#' @export
launch_dashboard <- function(open_viewer = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("`launch_dashboard()` requires the `shiny` package. Install it with install.packages('shiny').")
  }

  ensure_queue_initialized()
  write_dashboard_snapshot()
  invisible(launch_dashboard_background(open_viewer = open_viewer, announce = TRUE, focus_existing = TRUE))
}
