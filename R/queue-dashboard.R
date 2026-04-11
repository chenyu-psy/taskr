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
    shiny::div(class = "task-expand-meta", paste("Status:", task$status)),
    shiny::div(class = "task-expand-meta", paste("Priority:", task$priority)),
    shiny::div(class = "task-expand-meta", paste("Submitted:", task$submit_time_label)),
    shiny::div(class = "task-expand-meta", paste("Started:", task$start_time_label)),
    shiny::div(class = "task-expand-meta", paste("Ended:", task$end_time_label)),
    shiny::div(class = "task-expand-meta", paste("Error:", ifelse(nzchar(task$error %||% ""), task$error, "-"))),
    shiny::tags$pre(
      class = "task-expand-logs",
      `data-scroll-key` = sprintf("logs-%s", task$id),
      log_text
    )
  )
}

running_task_card_ui <- function(task, expanded = FALSE, allow_cancel = TRUE) {
  select_id <- button_id_for_task("select", task$id)
  cancel_id <- button_id_for_task("cancel", task$id)
  progress_ratio <- normalize_progress_fraction(task$progress, default_fraction = 0.5)
  chain_tab <- dashboard_stan_chain_progress_from_row(task)
  start_epoch <- if (is.na(task$start_time)) NA_real_ else as.numeric(as.POSIXct(task$start_time))
  start_epoch_attr <- if (is.na(start_epoch)) "" else sprintf("%.6f", start_epoch)

  shiny::div(
    class = dashboard_card_class(task$status),
    shiny::div(
      class = "task-card-head",
      shiny::div(class = "task-card-title", task$label),
      shiny::span(class = paste("status-pill", status_badge_class(task$status)), task$status)
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
    shiny::div(class = "task-card-msg", task$message %||% ""),
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
    shiny::div(
      class = "task-card-head",
      shiny::div(class = "task-card-title", task$label),
      shiny::span(class = paste("status-pill", status_badge_class(task$status)), task$status)
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

# Render one terminal-task card (done/failed/killed).
# Args:
# - task: One-row data.frame for a terminal task.
# Returns:
# - A `shiny::div` card tag.
done_task_card_ui <- function(task, expanded = FALSE) {
  select_id <- button_id_for_task("select", task$id)

  shiny::div(
    class = dashboard_card_class(task$status),
    shiny::div(
      class = "task-card-head",
      shiny::div(class = "task-card-title", task$label),
      shiny::span(class = paste("status-pill", status_badge_class(task$status)), task$status)
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

done_filter_controls_ui <- function(done_n, failed_n, killed_n, selected = "done") {
  btn_class <- function(key) {
    base <- "btn btn-sm done-filter-btn"
    if (identical(selected, key)) {
      return(paste(base, "active"))
    }
    base
  }

  shiny::div(
    class = "done-filter-row",
    shiny::actionButton("done_filter_done", sprintf("done (%d)", done_n), class = btn_class("done")),
    shiny::actionButton("done_filter_failed", sprintf("failed (%d)", failed_n), class = btn_class("failed")),
    shiny::actionButton("done_filter_killed", sprintf("killed (%d)", killed_n), class = btn_class("killed"))
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
      ".status-done { background: #edf6ff; color: #1f5f99; border-color: #b9d7f4; }",
      ".status-failed { background: #fdecec; color: #9e1c1c; border-color: #efb5b5; }",
      ".status-killed { background: #fff3e5; color: #9a5a00; border-color: #f2d2a3; }",
      ".done-filter-row { display: flex; gap: 8px; margin-bottom: 10px; flex-wrap: wrap; }",
      ".done-filter-btn { border: 1px solid #cfd8e2; background: #f7f9fc; color: #304558; }",
      ".done-filter-btn.active { background: #2d6cdf; color: #fff; border-color: #2d6cdf; }",
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
            shiny::fluidRow(
              shiny::column(
                width = 8,
                shiny::h3("Task Monitor")
              ),
                shiny::column(
                  width = 4,
                  shiny::textInput("task_query", "Search", value = "", placeholder = "label or task id")
                )
            ),
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
        shiny::column(width = 12, shiny::div(class = "block-card", shiny::uiOutput("done_cards")))
      )
    )
  )
}

queue_dashboard_app <- function(data_mode = c("live", "snapshot"), snapshot_path = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("`queue_dashboard()` requires the `shiny` package. Install it with install.packages('shiny').")
  }

  data_mode <- match.arg(data_mode)
  read_only <- identical(data_mode, "snapshot")
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
      done_filter_status <- shiny::reactiveVal("done")
      click_counts <- new.env(parent = emptyenv())
      click_counts$values <- list()
      read_snapshot_tasks <- function() {
        out <- tryCatch(read_dashboard_snapshot(path = snapshot_path), error = function(e) NULL)
        if (is.null(out)) {
          return(list(tasks = empty_dashboard_table(), max_concurrent = 1L))
        }
        list(
          tasks = out$tasks %||% empty_dashboard_table(),
          max_concurrent = as.integer(out$max_concurrent %||% 1L)
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
          expanded_id <- selected_id()
          running_now <- tab[tab$status == "running", , drop = FALSE]
          expanded_is_running <- !is.null(expanded_id) && expanded_id %in% running_now$id

          if (isTRUE(expanded_is_running)) {
            # While reading an expanded running task, ignore progress/log-only
            # deltas so the card is not rebuilt every second.
            if (nrow(running_now) == 0) {
              structure_sig <- "running-structure:empty"
            } else {
              structure_tab <- data.frame(
                id = as.character(running_now$id),
                status = as.character(running_now$status),
                start_time = as.numeric(running_now$start_time),
                end_time = as.numeric(running_now$end_time),
                stringsAsFactors = FALSE
              )
              structure_tab <- structure_tab[order(structure_tab$id), , drop = FALSE]
              structure_sig <- paste(
                "running-structure",
                paste(apply(structure_tab, 1, function(row) paste(row, collapse = "|")), collapse = ";"),
                sep = ":"
              )
            }
            return(paste0(structure_sig, "-", refresh_nonce()))
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
          read_snapshot_tasks()$max_concurrent %||% 1L
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
            label = sprintf("Completion: %d / %d terminal", summary$done + summary$failed + summary$killed, summary$total)
          )
        )
      })

      output$running_cards <- shiny::renderUI({
        expanded_task_id <- selected_id()
        column_cards_ui(
          filtered_running_tasks(),
          title = "Running",
          renderer = function(task) {
            running_task_card_ui(
              task,
              expanded = identical(task$id, expanded_task_id),
              allow_cancel = !isTRUE(read_only)
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
              task,
              expanded = identical(task$id, expanded_task_id),
              allow_cancel = !isTRUE(read_only)
            )
          }
        )
      })

      output$done_cards <- shiny::renderUI({
        expanded_task_id <- selected_id()
        done_all <- state_split()$done
        done_n <- sum(done_all$status == "done")
        failed_n <- sum(done_all$status == "failed")
        killed_n <- sum(done_all$status == "killed")
        selected_filter <- done_filter_status()
        done_filtered <- done_all[done_all$status == selected_filter, , drop = FALSE]

        column_cards_ui(
          done_filtered,
          title = "Finished",
          renderer = function(task) done_task_card_ui(task, expanded = identical(task$id, expanded_task_id)),
          header_right = if (!isTRUE(read_only)) {
            shiny::actionButton("clean_finished", "Clean Finished", class = "btn btn-warning btn-sm")
          },
          subheader = done_filter_controls_ui(
            done_n = done_n,
            failed_n = failed_n,
            killed_n = killed_n,
            selected = selected_filter
          )
        )
      })

      shiny::observeEvent(input$done_filter_done, {
        done_filter_status("done")
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$done_filter_failed, {
        done_filter_status("failed")
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$done_filter_killed, {
        done_filter_status("killed")
      }, ignoreInit = TRUE)

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
              click_key <- paste(prefix, current_button_id, sep = "::")
              current_count <- suppressWarnings(as.integer(input[[current_button_id]] %||% 0L))
              if (is.na(current_count)) {
                current_count <- 0L
              }
              previous_count <- as.integer(click_counts$values[[click_key]] %||% 0L)
              if (current_count <= previous_count) {
                return(invisible(NULL))
              }
              click_counts$values[[click_key]] <- current_count
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
          state_split()$done$id
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
          ids = if (isTRUE(read_only)) character() else ids,
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
            invisible(NULL)
          }
        )

        invisible(NULL)
      })

      shiny::observeEvent(input$confirm_running_cancel, {
        if (isTRUE(read_only)) {
          return(invisible(NULL))
        }
        task_id <- pending_running_cancel()
        shiny::removeModal()
        if (is.null(task_id)) {
          return(invisible(NULL))
        }

        tryCatch(
          {
            cancel_task(task_id)
            shiny::showNotification("Running task canceled.", type = "message")
          },
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error")
          }
        )
        pending_running_cancel(NULL)
        invisible(NULL)
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$clean_finished, {
        if (isTRUE(read_only)) {
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

      shiny::observe({
        # Keep expanded id valid when data updates.
        current_id <- selected_id()
        if (is.null(current_id)) return(invisible(NULL))

        visible_ids <- unique(c(
          filtered_running_tasks()$id,
          state_split()$queued$id,
          state_split()$done$id
        ))
        if (length(visible_ids) == 0) {
          selected_id(NULL)
          return(invisible(NULL))
        }
        if (!current_id %in% visible_ids) {
          selected_id(NULL)
        }
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
#' \donttest{
#' init_queue(max_concurrent = 2)
#' queue_dashboard()
#' }
#' @export
queue_dashboard <- function(open_viewer = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("`queue_dashboard()` requires the `shiny` package. Install it with install.packages('shiny').")
  }

  ensure_queue_initialized()
  write_dashboard_snapshot()
  invisible(launch_dashboard_background(open_viewer = open_viewer, announce = TRUE, focus_existing = TRUE))
}
