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
    `data-taskr-detail` = as.character(task$id),
    shiny::div(class = "task-expand-meta js-detail-status", paste("Status:", status_display_label(task$status))),
    shiny::div(class = "task-expand-meta js-detail-priority", paste("Priority:", task$priority)),
    shiny::div(class = "task-expand-meta js-detail-submit", paste("Submitted:", task$submit_time_label)),
    shiny::div(class = "task-expand-meta js-detail-start", paste("Started:", task$start_time_label)),
    shiny::div(class = "task-expand-meta js-detail-end", paste("Ended:", task$end_time_label)),
    shiny::tags$pre(
      id = dashboard_log_dom_id(task$id),
      class = "task-expand-logs",
      `data-scroll-key` = sprintf("logs-%s", task$id),
      `data-log-task-id` = as.character(task$id),
      log_text
    )
  )
}

# Render one dashboard action button without a Shiny input id.
#
# Purpose:
# - Avoid duplicate input-id warnings when Shiny replaces dynamic card lists.
# - Send all card clicks through one delegated browser event.
#
# Args:
# - action: Short action key, for example "select" or "cancel".
# - task_id: Task id attached to the card.
# - label: Button text shown to the user.
# - class: CSS classes for Bootstrap styling.
# Returns:
# - A plain HTML `<button>` tag.
dashboard_action_button_ui <- function(action, task_id, label, class) {
  shiny::tags$button(
    type = "button",
    class = class,
    `data-taskr-action` = as.character(action),
    `data-taskr-task-id` = as.character(task_id),
    label
  )
}

task_progress_area_ui <- function(chain_tab = NULL, progress_ratio = 0.5) {
  if (!is.null(chain_tab) && nrow(chain_tab) > 0) {
    return(shiny::div(
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
    ))
  }

  shiny::div(
    class = "task-progress-track",
    shiny::div(
      class = "task-progress-fill",
      style = sprintf("width: %.1f%%;", 100 * normalize_progress_fraction(progress_ratio, default_fraction = 0.5))
    )
  )
}

running_task_card_ui <- function(
    task,
    expanded = FALSE,
    allow_cancel = TRUE,
    allow_remove = FALSE,
    chain_tab = NULL,
    progress_ratio = NULL) {
  if (is.null(progress_ratio) || length(progress_ratio) == 0 || is.na(progress_ratio)) {
    progress_ratio <- normalize_progress_fraction(task$progress, default_fraction = 0.5)
  } else {
    progress_ratio <- normalize_progress_fraction(progress_ratio, default_fraction = 0.5)
  }
  if (is.null(chain_tab)) {
    chain_tab <- dashboard_stan_chain_progress_from_row(task)
  }
  msg <- as.character(task$message %||% "")
  if (length(msg) == 0 || is.na(msg)) {
    msg <- ""
  }
  show_msg <- nzchar(trimws(msg))
  start_epoch <- if (is.na(task$start_time)) NA_real_ else as.numeric(as.POSIXct(task$start_time))
  start_epoch_attr <- if (is.na(start_epoch)) "" else sprintf("%.6f", start_epoch)

  shiny::div(
    id = dashboard_card_dom_id(task$id),
    class = dashboard_card_class(task$status),
    `data-task-id` = as.character(task$id),
    `data-task-panel` = "running",
    shiny::div(
      class = "task-card-head",
      shiny::div(class = "task-card-title js-task-card-title", task$card_title),
      shiny::span(
        class = paste("status-pill", status_badge_class(task$status), "js-task-card-summary js-running-elapsed"),
        `data-start-epoch` = start_epoch_attr,
        task$card_summary
      )
    ),
    shiny::div(class = "task-card-meta js-task-card-meta", paste("Started:", task$start_time_label)),
    shiny::div(
      class = "task-card-msg js-task-card-msg",
      style = if (isTRUE(show_msg)) NULL else "display: none;",
      msg
    ),
    shiny::div(
      class = "js-task-progress-area",
      task_progress_area_ui(chain_tab = chain_tab, progress_ratio = progress_ratio)
    ),
    shiny::div(
      class = "task-card-actions",
      dashboard_action_button_ui(
        action = "select",
        task_id = task$id,
        label = ifelse(expanded, "Collapse", "Details"),
        class = "btn btn-sm btn-outline-primary"
      ),
      if (isTRUE(allow_cancel)) {
        dashboard_action_button_ui(
          action = "cancel",
          task_id = task$id,
          label = "Cancel",
          class = "btn btn-sm btn-outline-danger"
        )
      },
      if (isTRUE(allow_remove)) {
        dashboard_action_button_ui(
          action = "remove",
          task_id = task$id,
          label = "Remove",
          class = "btn btn-sm btn-outline-danger"
        )
      }
    ),
    task_expand_block_ui(task, expanded = expanded)
  )
}

# Render one compact pending-task card.
# Args:
# - task: One-row data.frame for a pending task.
# Returns:
# - A `shiny::div` card tag.
pending_task_card_ui <- function(task, expanded = FALSE, allow_cancel = TRUE, allow_remove = FALSE) {
  shiny::div(
    id = dashboard_card_dom_id(task$id),
    class = dashboard_card_class(task$status),
    `data-task-id` = as.character(task$id),
    `data-task-panel` = "pending",
    shiny::div(
      class = "task-card-head",
      shiny::div(class = "task-card-title js-task-card-title", task$card_title),
      shiny::span(class = paste("status-pill", status_badge_class(task$status), "js-task-card-summary"), task$card_summary)
    ),
    shiny::div(class = "task-card-meta js-task-card-meta", paste("Submitted:", task$submit_time_label)),
    shiny::div(class = "task-card-msg js-task-card-msg", style = "display: none;", ""),
    shiny::div(class = "js-task-progress-area"),
    shiny::div(
      class = "task-card-actions",
      dashboard_action_button_ui(
        action = "select",
        task_id = task$id,
        label = ifelse(expanded, "Collapse", "Details"),
        class = "btn btn-sm btn-outline-primary"
      ),
      if (isTRUE(allow_cancel)) {
        dashboard_action_button_ui(
          action = "cancel",
          task_id = task$id,
          label = "Cancel",
          class = "btn btn-sm btn-outline-danger"
        )
      },
      if (isTRUE(allow_remove)) {
        dashboard_action_button_ui(
          action = "remove",
          task_id = task$id,
          label = "Remove",
          class = "btn btn-sm btn-outline-danger"
        )
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
finished_task_card_ui <- function(task, expanded = FALSE, allow_remove = TRUE) {
  shiny::div(
    id = dashboard_card_dom_id(task$id),
    class = dashboard_card_class(task$status),
    `data-task-id` = as.character(task$id),
    `data-task-panel` = "finished",
    shiny::div(
      class = "task-card-head",
      shiny::div(class = "task-card-title js-task-card-title", task$card_title),
      shiny::span(class = paste("status-pill", status_badge_class(task$status), "js-task-card-summary"), task$card_summary)
    ),
    shiny::div(class = "task-card-meta js-task-card-meta", paste("Ended:", task$end_time_label)),
    shiny::div(class = "task-card-msg js-task-card-msg", style = "display: none;", ""),
    shiny::div(class = "js-task-progress-area"),
    shiny::div(
      class = "task-card-error js-task-card-error",
      style = if (identical(task$status, "failed") && nzchar(task$error %||% "")) NULL else "display: none;",
      task$error %||% ""
    ),
    shiny::div(
      class = "task-card-actions",
      dashboard_action_button_ui(
        action = "select",
        task_id = task$id,
        label = ifelse(expanded, "Collapse", "Details"),
        class = "btn btn-sm btn-outline-primary"
      ),
      if (isTRUE(allow_remove)) {
        dashboard_action_button_ui(
          action = "remove",
          task_id = task$id,
          label = "Remove",
          class = "btn btn-sm btn-outline-danger"
        )
      }
    ),
    task_expand_block_ui(task, expanded = expanded)
  )
}

# Render one dashboard column shell with stable scroll container.
# Args:
# - header_output_id: Output id for the column header UI.
# - body_dom_id: Static DOM id for persistent task cards.
# - scroll_key: Stable key used by focus and scrollable column lookup.
# - subheader_output_id: Optional output id for controls below header.
# Returns:
# - A section wrapper tag.
dashboard_column_shell_ui <- function(header_output_id, body_dom_id, scroll_key, subheader_output_id = NULL) {
  shiny::div(
    class = "dashboard-column",
    shiny::uiOutput(header_output_id),
    if (!is.null(subheader_output_id)) shiny::uiOutput(subheader_output_id),
    shiny::div(
      class = "dashboard-column-body",
      `data-scroll-key` = scroll_key,
      shiny::div(
        id = body_dom_id,
        class = "dashboard-column-cards",
        `data-taskr-panel` = sub("^taskr-(.*)-cards$", "\\1", body_dom_id),
        shiny::div(class = "empty-block js-empty-block", "No tasks")
      )
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
      ".dashboard-column-cards { width: 100%; display: flex; flex-direction: column; gap: 8px; }",
      ".task-card { border: 1px solid #d7e0e8; border-radius: 8px; padding: 9px; background: #fbfdff; }",
      ".task-card { display: flex; flex-direction: column; gap: 4px; }",
      ".task-card { width: min(100%, 980px); }",
      ".task-card-pending { border-color: #2d6cdf; }",
      ".task-card-running { border-color: #1f7a5b; }",
      ".task-card-completed { border-color: #d9a21b; }",
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
      ".status-pending { background: #edf4ff; color: #1f5f99; border-color: #b9d7f4; }",
      ".status-completed { background: #fff8e8; color: #8a5d00; border-color: #ead08a; }",
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

# Provide JavaScript used by the persistent dashboard card runtime.
# Returns:
# - One `<script>` tag.
dashboard_scroll_js <- function() {
  shiny::tags$script(shiny::HTML(
    paste(
      "(function () {",
      "  if (window.__taskrDashboardRuntime) return;",
      "  window.__taskrDashboardRuntime = true;",
      "  window.__taskrLogStickToBottom = {};",
      "  function isLogNode(el) {",
      "    return !!(el && el.classList && el.classList.contains('task-expand-logs') && el.getAttribute('data-log-task-id'));",
      "  }",
      "  function logKey(node) {",
      "    if (!node || !node.getAttribute) return '';",
      "    return node.getAttribute('data-scroll-key') || ('logs-' + String(node.getAttribute('data-log-task-id') || ''));",
      "  }",
      "  function logBottomGap(node) {",
      "    return node.scrollHeight - (node.scrollTop + node.clientHeight);",
      "  }",
      "  function setLogStick(node, value) {",
      "    var key = logKey(node);",
      "    if (key) window.__taskrLogStickToBottom[key] = !!value;",
      "  }",
      "  function logShouldStick(node) {",
      "    var key = logKey(node);",
      "    if (!key) return true;",
      "    if (!Object.prototype.hasOwnProperty.call(window.__taskrLogStickToBottom, key)) {",
      "      window.__taskrLogStickToBottom[key] = true;",
      "    }",
      "    return !!window.__taskrLogStickToBottom[key];",
      "  }",
      "  function rememberLogStick(node) {",
      "    if (!isLogNode(node)) return;",
      "    setLogStick(node, logBottomGap(node) <= 16);",
      "  }",
      "  function scrollLogToBottom(node) {",
      "    if (!node) return;",
      "    node.scrollTop = node.scrollHeight;",
      "    setLogStick(node, true);",
      "  }",
      "  document.addEventListener('scroll', function (evt) {",
      "    var el = evt.target;",
      "    if (!el || !el.getAttribute) return;",
      "    rememberLogStick(el);",
      "  }, true);",
      "  function cardFor(taskId) { return document.getElementById('taskr-card-' + String(taskId)); }",
      "  function panelFor(panel) { return document.getElementById('taskr-' + String(panel) + '-cards'); }",
      "  function setText(root, selector, value) {",
      "    var node = root && root.querySelector ? root.querySelector(selector) : null;",
      "    if (node) node.textContent = value == null ? '' : String(value);",
      "  }",
      "  function setMaybeText(root, selector, value) {",
      "    var node = root && root.querySelector ? root.querySelector(selector) : null;",
      "    if (!node) return;",
      "    var text = value == null ? '' : String(value);",
      "    node.textContent = text;",
      "    node.style.display = text.trim() ? '' : 'none';",
      "  }",
      "  function updateEmpty(panelNode) {",
      "    if (!panelNode || !panelNode.querySelectorAll) return;",
      "    var empty = panelNode.querySelector('.js-empty-block');",
      "    if (!empty) return;",
      "    var cards = panelNode.querySelectorAll('.task-card[data-task-id]');",
      "    empty.style.display = cards.length ? 'none' : '';",
      "  }",
      "  function updateAllEmptyBlocks() {",
      "    ['running', 'pending', 'finished'].forEach(function (panel) { updateEmpty(panelFor(panel)); });",
      "  }",
      "  function applyPanelOrder(panel, ids) {",
      "    var panelNode = panelFor(panel);",
      "    if (!panelNode) return;",
      "    (ids || []).forEach(function (taskId) {",
      "      var card = cardFor(taskId);",
      "      if (card && card.parentNode !== panelNode) panelNode.appendChild(card);",
      "      if (card && card.parentNode === panelNode) panelNode.appendChild(card);",
      "    });",
      "    updateEmpty(panelNode);",
      "  }",
      "  function updateDetail(card, msg) {",
      "    if (!card) return;",
      "    var detail = card.querySelector('.task-expand-block[data-taskr-detail]');",
      "    var expanded = !!(msg && msg.expanded);",
      "    if (!expanded) {",
      "      if (detail && detail.parentNode) detail.parentNode.removeChild(detail);",
      "      return;",
      "    }",
      "    if (!detail && msg.detail_html) {",
      "      card.insertAdjacentHTML('beforeend', String(msg.detail_html));",
      "      detail = card.querySelector('.task-expand-block[data-taskr-detail]');",
      "    }",
      "    if (!detail) return;",
      "    setText(detail, '.js-detail-status', msg.detail_status || '');",
      "    setText(detail, '.js-detail-priority', msg.detail_priority || '');",
      "    setText(detail, '.js-detail-submit', msg.detail_submit || '');",
      "    setText(detail, '.js-detail-start', msg.detail_start || '');",
      "    setText(detail, '.js-detail-end', msg.detail_end || '');",
      "  }",
      "  function updateCard(msg) {",
      "    var taskId = msg && msg.id != null ? String(msg.id) : '';",
      "    var card = taskId ? cardFor(taskId) : null;",
      "    if (!card) return;",
      "    if (msg.card_class) card.className = String(msg.card_class);",
      "    if (msg.panel) card.setAttribute('data-task-panel', String(msg.panel));",
      "    setText(card, '.js-task-card-title', msg.title || '');",
      "    setText(card, '.js-task-card-meta', msg.meta || '');",
      "    setMaybeText(card, '.js-task-card-msg', msg.message || '');",
      "    setMaybeText(card, '.js-task-card-error', msg.error || '');",
      "    var summary = card.querySelector('.js-task-card-summary');",
      "    if (summary) {",
      "      summary.className = String(msg.summary_class || 'status-pill js-task-card-summary');",
      "      summary.textContent = msg.summary == null ? '' : String(msg.summary);",
      "      if (msg.start_epoch != null) summary.setAttribute('data-start-epoch', String(msg.start_epoch));",
      "      if (String(msg.panel || '') === 'running') summary.classList.add('js-running-elapsed');",
      "      else summary.classList.remove('js-running-elapsed');",
      "    }",
      "    var progress = card.querySelector('.js-task-progress-area');",
      "    if (progress) {",
      "      if (String(msg.panel || '') === 'running' && msg.progress_html != null) {",
      "        progress.innerHTML = String(msg.progress_html);",
      "      } else {",
      "        progress.innerHTML = '';",
      "      }",
      "    }",
      "    var select = card.querySelector('[data-taskr-action=\"select\"]');",
      "    if (select) select.textContent = msg.expanded ? 'Collapse' : 'Details';",
      "    updateDetail(card, msg);",
      "  }",
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
      "  document.addEventListener('shiny:connected', function () { updateRunningElapsed(document); });",
      "  document.addEventListener('click', function (evt) {",
      "    var target = evt.target;",
      "    var btn = target && target.closest ? target.closest('[data-taskr-action]') : null;",
      "    if (!btn || btn.disabled) return;",
      "    var action = btn.getAttribute('data-taskr-action') || '';",
      "    var taskId = btn.getAttribute('data-taskr-task-id') || '';",
      "    if (!action || !taskId) return;",
      "    evt.preventDefault();",
      "    if (!window.Shiny || !window.Shiny.setInputValue) return;",
      "    window.Shiny.setInputValue('taskr_card_action', {",
      "      action: action,",
      "      task_id: taskId,",
      "      nonce: Date.now() + ':' + Math.random()",
      "    }, { priority: 'event' });",
      "  });",
      "  function registerTaskrShinyHandlers() {",
      "    if (window.__taskrShinyHandlersRegistered) return;",
      "    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) {",
      "      setTimeout(registerTaskrShinyHandlers, 50);",
      "      return;",
      "    }",
      "    window.__taskrShinyHandlersRegistered = true;",
      "    window.Shiny.addCustomMessageHandler('taskr_reconcile_cards', function (msg) {",
      "      var panels = msg && msg.panels ? msg.panels : {};",
      "      applyPanelOrder('running', panels.running || []);",
      "      applyPanelOrder('pending', panels.pending || []);",
      "      applyPanelOrder('finished', panels.finished || []);",
      "      updateAllEmptyBlocks();",
      "    });",
      "    window.Shiny.addCustomMessageHandler('taskr_update_cards', function (msg) {",
      "      var cards = msg && msg.cards ? msg.cards : [];",
      "      Array.prototype.forEach.call(cards, updateCard);",
      "      updateRunningElapsed(document);",
      "      updateAllEmptyBlocks();",
      "    });",
      "    window.Shiny.addCustomMessageHandler('taskr_update_logs', function (msg) {",
      "      var taskId = msg && msg.task_id ? String(msg.task_id) : '';",
      "      if (!taskId || !document.querySelector) return;",
      "      var node = document.getElementById('taskr-log-' + taskId) || document.querySelector('[data-log-task-id=\"' + taskId + '\"]');",
      "      if (!node) return;",
      "      var text = msg && Object.prototype.hasOwnProperty.call(msg, 'text') ? String(msg.text) : '';",
      "      var bottomGap = node.scrollHeight - (node.scrollTop + node.clientHeight);",
      "      var nearBottom = bottomGap <= 16;",
      "      var prevTop = node.scrollTop;",
      "      var shouldStickToBottom = nearBottom || logShouldStick(node);",
      "      node.textContent = text;",
      "      if (shouldStickToBottom) {",
      "        scrollLogToBottom(node);",
      "      } else {",
      "        node.scrollTop = prevTop;",
      "        rememberLogStick(node);",
      "      }",
      "    });",
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
      "    function forceCloseTaskrModal() {",
      "      var modals = document.querySelectorAll ? document.querySelectorAll('.modal') : [];",
      "      Array.prototype.forEach.call(modals, function (modal) {",
      "        modal.classList.remove('show');",
      "        modal.classList.remove('in');",
      "        modal.setAttribute('aria-hidden', 'true');",
      "        modal.style.display = 'none';",
      "      });",
      "      var backdrops = document.querySelectorAll ? document.querySelectorAll('.modal-backdrop') : [];",
      "      Array.prototype.forEach.call(backdrops, function (node) {",
      "        if (node && node.parentNode) node.parentNode.removeChild(node);",
      "      });",
      "      if (document.body && document.body.classList) {",
      "        document.body.classList.remove('modal-open');",
      "      }",
      "      if (document.body && document.body.style) {",
      "        document.body.style.removeProperty('padding-right');",
      "        document.body.style.removeProperty('overflow');",
      "      }",
      "    }",
      "    window.Shiny.addCustomMessageHandler('taskr_force_close_modal', function (msg) {",
      "      forceCloseTaskrModal();",
      "      setTimeout(forceCloseTaskrModal, 50);",
      "      setTimeout(forceCloseTaskrModal, 250);",
      "    });",
      "    window.Shiny.addCustomMessageHandler('taskr_control_request', function (msg) {",
      "      var requestId = msg && msg.request_id ? String(msg.request_id) : '';",
      "      var action = msg && msg.action ? String(msg.action) : '';",
      "      var taskId = msg && msg.task_id ? String(msg.task_id) : '';",
      "      var url = msg && msg.url ? String(msg.url) : '';",
      "      var endpoint = msg && msg.endpoint ? String(msg.endpoint) : '';",
      "      var token = msg && msg.token ? String(msg.token) : '';",
      "      if (!requestId || !url || !endpoint) return;",
      "      var payload = { token: token };",
      "      if (taskId) payload.task_id = taskId;",
      "      if (window.console && window.console.debug) {",
      "        window.console.debug('[taskr] control request', action, taskId, url + endpoint);",
      "      }",
      "      var done = false;",
      "      var controller = window.AbortController ? new AbortController() : null;",
      "      var timer = setTimeout(function () {",
      "        if (done) return;",
      "        if (controller) controller.abort();",
      "      }, 10000);",
      "      fetch(url + endpoint, {",
      "        method: 'POST',",
      "        headers: { 'Content-Type': 'text/plain' },",
      "        body: JSON.stringify(payload),",
      "        signal: controller ? controller.signal : undefined",
      "      }).then(function (response) {",
      "        return response.text().then(function (text) {",
      "          var data = {};",
      "          try { data = text ? JSON.parse(text) : {}; } catch (e) { data = { ok: false, error: text || String(e) }; }",
      "          data.http_status = response.status;",
      "          data.request_id = requestId;",
      "          data.action = action;",
      "          data.task_id = taskId;",
      "          if (!response.ok && data.ok !== false) data.ok = false;",
      "          return data;",
      "        });",
      "      }).catch(function (err) {",
      "        return { ok: false, request_id: requestId, action: action, task_id: taskId, error: String(err) };",
      "      }).then(function (data) {",
      "        done = true;",
      "        clearTimeout(timer);",
      "        if (window.console && window.console.debug) {",
      "          window.console.debug('[taskr] control result', data);",
      "        }",
      "        if (window.Shiny && window.Shiny.setInputValue) {",
      "          window.Shiny.setInputValue('taskr_control_result', data, { priority: 'event' });",
      "        }",
      "      });",
      "    });",
      "  }",
      "  registerTaskrShinyHandlers();",
      "  document.addEventListener('shiny:connected', registerTaskrShinyHandlers);",
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
              shiny::textInput("task_query", "Search", value = "", placeholder = "task id or name"),
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
            dashboard_column_shell_ui(
              header_output_id = "running_header",
              body_dom_id = dashboard_panel_dom_id("running"),
              scroll_key = "col-running"
            )
          )
        )
      ),
      shiny::fluidRow(
        class = "dashboard-row",
        shiny::column(
          width = 12,
          shiny::div(
            class = "block-card",
            dashboard_column_shell_ui(
              header_output_id = "pending_header",
              body_dom_id = dashboard_panel_dom_id("pending"),
              scroll_key = "col-pending"
            )
          )
        )
      ),
      shiny::fluidRow(
        class = "dashboard-row",
        shiny::column(
          width = 12,
          shiny::div(
            class = "block-card",
            dashboard_column_shell_ui(
              header_output_id = "finished_header",
              subheader_output_id = "finished_subheader",
              body_dom_id = dashboard_panel_dom_id("finished"),
              scroll_key = "col-finished"
            )
          )
        )
      )
    )
  )
}

queue_dashboard_app <- function(
    data_mode = c("live", "snapshot"),
    snapshot_path = NULL,
    control_url = NULL,
    control_token = NULL,
    cancel_dir = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("`launch_dashboard()` requires the `shiny` package. Install it with install.packages('shiny').")
  }

  data_mode <- match.arg(data_mode)
  read_only <- identical(data_mode, "snapshot")
  control_via_server <- isTRUE(read_only) &&
    !is.null(control_url) &&
    is.character(control_url) &&
    length(control_url) == 1 &&
    !is.na(control_url) &&
    nzchar(control_url) &&
    !is.null(control_token) &&
    is.character(control_token) &&
    length(control_token) == 1 &&
    !is.na(control_token) &&
    nzchar(control_token)
  control_via_files <- isTRUE(read_only) &&
    !is.null(cancel_dir) &&
    is.character(cancel_dir) &&
    length(cancel_dir) == 1 &&
    !is.na(cancel_dir) &&
    nzchar(cancel_dir)
  can_control <- !isTRUE(read_only) || isTRUE(control_via_files) || isTRUE(control_via_server)
  can_remove <- !isTRUE(read_only) || isTRUE(control_via_server)
  if (!isTRUE(read_only)) {
    ensure_queue_initialized()
  }

  app <- shiny::shinyApp(
    ui = queue_dashboard_ui(),
    server = function(input, output, session) {
      selected_id <- shiny::reactiveVal(NULL)
      selected_panel_tracker <- shiny::reactiveVal(setNames(character(), character()))
      pending_running_cancel <- shiny::reactiveVal(NULL)
      pending_running_remove <- shiny::reactiveVal(NULL)
      refresh_nonce <- shiny::reactiveVal(0L)
      finished_filter_status <- shiny::reactiveVal("completed")
      display_progress_cache <- new.env(parent = emptyenv())
      pending_cancel_ids <- shiny::reactiveVal(character())
      pending_cancel_requests <- shiny::reactiveVal(setNames(character(), character()))
      action_cooldown <- shiny::reactiveVal(list())
      last_focus_target <- shiny::reactiveVal("")
      rendered_card_panels <- shiny::reactiveVal(setNames(character(), character()))
      read_snapshot_state <- function() {
        out <- tryCatch(read_dashboard_snapshot(path = snapshot_path), error = function(e) NULL)
        if (is.null(out)) {
          return(list(
            session_id = NA_character_,
            tasks = empty_dashboard_table(),
            max_slots = 1L,
            cancel_dir = cancel_dir %||% NA_character_
          ))
        }
        list(
          session_id = out$session_id %||% NA_character_,
          tasks = out$tasks %||% empty_dashboard_table(),
          max_slots = as.integer(out$max_slots %||% 1L),
          cancel_dir = out$cancel_dir %||% cancel_dir %||% NA_character_
        )
      }

      send_control_request <- function(action, task_id = NULL) {
        if (!isTRUE(control_via_server)) {
          shiny::showNotification("Dashboard control server is not available.", type = "error")
          return(NULL)
        }

        if (identical(action, "cancel")) {
          endpoint <- "/cancel"
        } else if (identical(action, "remove")) {
          endpoint <- "/remove"
        } else if (identical(action, "clean_finished")) {
          endpoint <- "/clean_finished"
        } else {
          endpoint <- "/clear_all"
        }
        request_id <- new_dashboard_session_id()
        session$sendCustomMessage(
          "taskr_control_request",
          list(
            request_id = request_id,
            action = action,
            endpoint = endpoint,
            url = control_url,
            token = control_token,
            task_id = task_id %||% ""
          )
        )
        request_id
      }

      close_dashboard_modal <- function() {
        # Shiny sometimes leaves a Bootstrap backdrop behind when the modal
        # close and an async browser request are flushed together. Force a
        # front-end cleanup so the dashboard remains usable while cancel runs.
        shiny::removeModal()
        session$sendCustomMessage("taskr_force_close_modal", list())
        invisible(NULL)
      }

      # Kill a running task process immediately from the dashboard process.
      # Args:
      # - pid: Operating-system PID from the dashboard snapshot.
      # Returns:
      # - Invisibly returns TRUE when a plausible kill signal was sent.
      # Side effects:
      # - Prefer process-group kill so parallel child workers are released.
      # - Fall back to single-PID TERM/KILL when group lookup is unavailable.
      kill_dashboard_pid <- function(pid) {
        pid <- suppressWarnings(as.integer(pid))
        if (length(pid) != 1 || is.na(pid) || pid < 1L) {
          return(invisible(FALSE))
        }

        pgid <- task_process_group_id(pid)
        if (!is.na(pgid) && task_kill_process_group(pgid = pgid, timeout_sec = 0.1)) {
          return(invisible(TRUE))
        }

        try(tools::pskill(pid, signal = tools::SIGTERM), silent = TRUE)
        Sys.sleep(0.1)
        try(tools::pskill(pid, signal = tools::SIGKILL), silent = TRUE)
        invisible(TRUE)
      }

      # Request cancellation without waiting for the main R console.
      # Args:
      # - task_id: Task id to cancel.
      # - row: Optional one-row dashboard snapshot, used to find the PID.
      # Returns:
      # - TRUE when the file-based control path is available, otherwise FALSE.
      # Side effects:
      # - Writes a cancel marker consumed by the scheduler process.
      # - Also sends immediate kill signals to reduce CPU usage quickly.
      request_local_cancel <- function(task_id, row = NULL) {
        if (!isTRUE(control_via_files)) {
          return(FALSE)
        }

        write_dashboard_cancel_marker(task_id = task_id, cancel_dir = cancel_dir)
        if (!is.null(row) && nrow(row) > 0 && "pid" %in% names(row)) {
          kill_dashboard_pid(row$pid[[1]])
        }
        TRUE
      }

      # Request finished-record cleanup through the same marker directory used
      # for robust dashboard cancellation.
      # Args:
      # - None.
      # Returns:
      # - TRUE when the file-based control path is available, otherwise FALSE.
      # Side effects:
      # - Writes a cleanup marker consumed by the main scheduler tick.
      request_local_clean_finished <- function() {
        if (!isTRUE(control_via_files)) {
          return(FALSE)
        }

        write_dashboard_clean_finished_marker(cancel_dir = cancel_dir)
        TRUE
      }

      cached_display_progress <- function(task_id) {
        if (!exists(task_id, envir = display_progress_cache, inherits = FALSE)) {
          return(NA_real_)
        }
        suppressWarnings(as.numeric(get(task_id, envir = display_progress_cache, inherits = FALSE)))
      }

      remember_display_progress <- function(task_id, progress) {
        assign(task_id, as.numeric(progress), envir = display_progress_cache)
        invisible(NULL)
      }

      drop_stale_display_progress <- function(active_ids) {
        cached_ids <- ls(display_progress_cache, all.names = TRUE)
        stale_ids <- setdiff(cached_ids, as.character(active_ids))
        if (length(stale_ids) > 0) {
          rm(list = stale_ids, envir = display_progress_cache)
        }
        invisible(NULL)
      }

      apply_pending_cancel_status <- function(task) {
        task_id <- as.character(task$id[[1]])
        if (task_id %in% pending_cancel_ids() && task$status[[1]] %in% c("running", "pending")) {
          task$status[[1]] <- "canceling"
        }
        task
      }

      dashboard_card_meta_text <- function(task, panel) {
        if (identical(panel, "running")) {
          return(paste("Started:", task$start_time_label))
        }
        if (identical(panel, "pending")) {
          return(paste("Submitted:", task$submit_time_label))
        }
        paste("Ended:", task$end_time_label)
      }

      dashboard_card_payload <- function(task, panel, expanded = FALSE, chain_tab = NULL, progress_ratio = NULL) {
        task_id <- as.character(task$id[[1]])
        start_epoch <- if (is.na(task$start_time)) NA_real_ else as.numeric(as.POSIXct(task$start_time))
        start_epoch_attr <- if (is.na(start_epoch)) "" else sprintf("%.6f", start_epoch)
        status <- as.character(task$status[[1]])
        list(
          id = task_id,
          panel = panel,
          title = as.character(task$card_title[[1]] %||% ""),
          card_class = dashboard_card_class(status),
          summary = as.character(task$card_summary[[1]] %||% ""),
          summary_class = paste("status-pill", status_badge_class(status), "js-task-card-summary"),
          start_epoch = start_epoch_attr,
          meta = dashboard_card_meta_text(task, panel),
          message = as.character(task$message[[1]] %||% ""),
          error = as.character(task$error[[1]] %||% ""),
          progress_html = if (identical(panel, "running")) {
            as.character(task_progress_area_ui(chain_tab = chain_tab, progress_ratio = progress_ratio))
          } else {
            ""
          },
          expanded = isTRUE(expanded),
          detail_html = if (isTRUE(expanded)) as.character(task_expand_block_ui(task, expanded = TRUE)) else "",
          detail_status = paste("Status:", status_display_label(status)),
          detail_priority = paste("Priority:", task$priority),
          detail_submit = paste("Submitted:", task$submit_time_label),
          detail_start = paste("Started:", task$start_time_label),
          detail_end = paste("Ended:", task$end_time_label)
        )
      }

      dashboard_card_ui_for_panel <- function(task, panel, expanded = FALSE, chain_tab = NULL, progress_ratio = NULL) {
        if (identical(panel, "running")) {
          return(running_task_card_ui(
            task,
            expanded = expanded,
            allow_cancel = isTRUE(can_control),
            allow_remove = FALSE,
            chain_tab = chain_tab,
            progress_ratio = progress_ratio
          ))
        }
        if (identical(panel, "pending")) {
          return(pending_task_card_ui(
            task,
            expanded = expanded,
            allow_cancel = isTRUE(can_control),
            allow_remove = FALSE
          ))
        }
        finished_task_card_ui(
          task,
          expanded = expanded,
          allow_remove = isTRUE(can_remove)
        )
      }

      remember_pending_cancel <- function(task_id, request_id) {
        req <- pending_cancel_requests()
        req[[request_id]] <- task_id
        pending_cancel_requests(req)
        pending_cancel_ids(unique(c(pending_cancel_ids(), task_id)))
      }

      forget_pending_cancel <- function(request_id) {
        req <- pending_cancel_requests()
        task_id <- as.character(req[[request_id]] %||% "")
        req[[request_id]] <- NULL
        pending_cancel_requests(req)

        if (!nzchar(task_id)) {
          return(invisible(NULL))
        }

        still_pending <- task_id %in% as.character(unname(req))
        if (!isTRUE(still_pending)) {
          pending_cancel_ids(setdiff(pending_cancel_ids(), task_id))
        }
        invisible(NULL)
      }

      read_current_dashboard_tasks <- function() {
        if (isTRUE(read_only)) {
          return(read_snapshot_state()$tasks)
        }
        tryCatch(extract_dashboard_snapshot(now = Sys.time()), error = function(e) empty_dashboard_table())
      }

      state_tasks <- shiny::reactivePoll(
        intervalMillis = 1000,
        session = session,
        checkFunc = function() {
          tab <- read_current_dashboard_tasks()
          paste0(
            dashboard_state_signature(tab),
            "-",
            dashboard_running_content_signature(tab),
            "-",
            refresh_nonce()
          )
        },
        valueFunc = function() {
          tab <- read_current_dashboard_tasks()
          add_dashboard_derived_columns(tab, now = Sys.time())
        }
      )

      expanded_running_log_text <- shiny::reactivePoll(
        intervalMillis = 1000,
        session = session,
        checkFunc = function() {
          task_id <- selected_id()
          if (is.null(task_id)) {
            return("running-log:none")
          }
          tab <- read_current_dashboard_tasks()
          paste0(dashboard_running_log_signature(tab, task_id = task_id), "-", refresh_nonce())
        },
        valueFunc = function() {
          task_id <- selected_id()
          if (is.null(task_id)) {
            return(NULL)
          }

          tab <- add_dashboard_derived_columns(read_current_dashboard_tasks(), now = Sys.time())
          row <- tab[tab$id == task_id & tab$status == "running", , drop = FALSE]
          if (nrow(row) == 0) {
            return(NULL)
          }

          list(task_id = task_id, text = dashboard_log_text_from_row(row, tail_n = 120L))
        }
      )

      filtered_state_tasks <- shiny::reactive({
        filter_dashboard_tasks(state_tasks(), query = input$task_query %||% "")
      })

      filtered_running_tasks <- shiny::reactive({
        tab <- state_tasks()
        tab <- tab[tab$status == "running", , drop = FALSE]
        filter_dashboard_tasks(tab, query = input$task_query %||% "")
      })

      state_split <- shiny::reactive({
        split_dashboard_tasks(filtered_state_tasks())
      })

      visible_dashboard_cards <- shiny::reactive({
        expanded_task_id <- selected_id()
        now_ts <- Sys.time()
        fallback_wait_sec <- dashboard_initial_progress_wait_sec()

        running_tab <- filtered_running_tasks()
        chain_by_id <- list()
        progress_by_id <- list()

        for (i in seq_len(nrow(running_tab))) {
          task_row <- running_tab[i, , drop = FALSE]
          task_id <- as.character(task_row$id[[1]])
          task_progress <- as.numeric(task_row$progress[[1]])
          cached_progress <- cached_display_progress(task_id)

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
          remember_display_progress(task_id, resolved)
        }

        running_ids <- as.character(running_tab$id %||% character())
        drop_stale_display_progress(running_ids)

        pending_tab <- state_split()$pending
        finished_all <- state_split()$finished
        selected_filter <- finished_filter_status()
        finished_tab <- finished_all[finished_all$status == selected_filter, , drop = FALSE]

        panels <- list(
          running = as.character(running_tab$id %||% character()),
          pending = as.character(pending_tab$id %||% character()),
          finished = as.character(finished_tab$id %||% character())
        )

        cards <- list()
        add_card <- function(task, panel, chain_tab = NULL, progress_ratio = NULL) {
          task <- apply_pending_cancel_status(task)
          task_id <- as.character(task$id[[1]])
          expanded <- identical(task$id, expanded_task_id)
          cards[[length(cards) + 1L]] <<- list(
            id = task_id,
            panel = panel,
            ui = dashboard_card_ui_for_panel(
              task,
              panel = panel,
              expanded = expanded,
              chain_tab = chain_tab,
              progress_ratio = progress_ratio
            ),
            payload = dashboard_card_payload(
              task,
              panel = panel,
              expanded = expanded,
              chain_tab = chain_tab,
              progress_ratio = progress_ratio
            )
          )
        }

        for (i in seq_len(nrow(running_tab))) {
          task <- running_tab[i, , drop = FALSE]
          task_id <- as.character(task$id[[1]])
          add_card(
            task,
            panel = "running",
            chain_tab = chain_by_id[[task_id]],
            progress_ratio = progress_by_id[[task_id]]
          )
        }
        for (i in seq_len(nrow(pending_tab))) {
          add_card(pending_tab[i, , drop = FALSE], panel = "pending")
        }
        for (i in seq_len(nrow(finished_tab))) {
          add_card(finished_tab[i, , drop = FALSE], panel = "finished")
        }

        list(panels = panels, cards = cards)
      })

      shiny::observe({
        tab <- state_tasks()
        current_panel <- setNames(character(), character())

        if (nrow(tab) > 0) {
          status <- as.character(tab$status %||% character())
          panel <- ifelse(
            status %in% c("completed", "failed", "cancelled"),
            "finished",
            status
          )
          current_panel <- stats::setNames(as.character(panel), as.character(tab$id))
        }

        previous_panel <- selected_panel_tracker()
        moved_ids <- intersect(names(previous_panel), names(current_panel))
        moved_ids <- moved_ids[previous_panel[moved_ids] != current_panel[moved_ids]]

        if (length(moved_ids) > 0) {
          current_selected <- selected_id()
          if (!is.null(current_selected) && as.character(current_selected) %in% moved_ids) {
            selected_id(NULL)
          }
        }

        selected_panel_tracker(current_panel)
      })

      output$summary_progress <- shiny::renderUI({
        slots <- if (isTRUE(read_only)) {
          read_snapshot_state()$max_slots %||% 1L
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

      output$running_header <- shiny::renderUI({
        running_n <- nrow(filtered_running_tasks())
        shiny::div(
          class = "dashboard-column-header",
          shiny::div(class = "dashboard-column-title", sprintf("Running (%d)", running_n))
        )
      })

      output$pending_header <- shiny::renderUI({
        pending_n <- nrow(state_split()$pending)
        shiny::div(
          class = "dashboard-column-header",
          shiny::div(class = "dashboard-column-title", sprintf("Pending (%d)", pending_n))
        )
      })

      output$finished_header <- shiny::renderUI({
        finished_all <- state_split()$finished
        selected_filter <- finished_filter_status()
        finished_n <- sum(finished_all$status == selected_filter)
        header_right <- if (isTRUE(can_control)) {
          shiny::actionButton("clean_finished", "Clean Finished", class = "btn btn-warning btn-sm")
        } else {
          NULL
        }
        shiny::div(
          class = "dashboard-column-header",
          shiny::div(class = "dashboard-column-title", sprintf("Finished (%d)", finished_n)),
          header_right
        )
      })

      output$finished_subheader <- shiny::renderUI({
        finished_all <- state_split()$finished
        completed_n <- sum(finished_all$status == "completed")
        failed_n <- sum(finished_all$status == "failed")
        cancelled_n <- sum(finished_all$status == "cancelled")
        selected_filter <- finished_filter_status()
        finished_filter_controls_ui(
          completed_n = completed_n,
          failed_n = failed_n,
          cancelled_n = cancelled_n,
          selected = selected_filter
        )
      })

      shiny::observe({
        model <- visible_dashboard_cards()
        cards <- model$cards
        desired <- setNames(
          vapply(cards, function(card) card$panel, character(1)),
          vapply(cards, function(card) card$id, character(1))
        )
        previous <- rendered_card_panels()
        stale_ids <- union(
          setdiff(names(previous), names(desired)),
          names(desired)[names(desired) %in% names(previous) & previous[names(desired)] != desired]
        )

        for (task_id in stale_ids) {
          shiny::removeUI(
            selector = dashboard_css_id_selector(dashboard_card_dom_id(task_id)),
            immediate = TRUE,
            session = session
          )
        }

        for (card in cards) {
          was_rendered <- card$id %in% names(previous) && identical(previous[[card$id]], card$panel)
          if (!isTRUE(was_rendered)) {
            shiny::insertUI(
              selector = dashboard_css_id_selector(dashboard_panel_dom_id(card$panel)),
              where = "beforeEnd",
              ui = card$ui,
              immediate = TRUE,
              session = session
            )
          }
        }

        rendered_card_panels(desired)
        payloads <- lapply(cards, function(card) card$payload)
        panels <- model$panels
        session$onFlushed(function() {
          session$sendCustomMessage("taskr_reconcile_cards", list(panels = panels))
          session$sendCustomMessage("taskr_update_cards", list(cards = payloads))
        }, once = TRUE)
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

      select_card_task <- function(task_id) {
        if (identical(selected_id(), task_id)) {
          selected_id(NULL)
        } else {
          selected_id(task_id)
        }
        invisible(NULL)
      }

      cancel_card_task <- function(task_id) {
        if (!isTRUE(can_control)) {
          return(invisible(NULL))
        }

        current <- state_tasks()
        row <- current[current$id == task_id, , drop = FALSE]
        if (nrow(row) == 0) {
          return(invisible(NULL))
        }

        if (identical(row$status[[1]], "running")) {
          pending_running_cancel(task_id)
          shiny::showModal(shiny::modalDialog(
            title = "Cancel running task?",
            sprintf("Task '%s' is running. Cancel now?", row$card_title[[1]]),
            footer = shiny::tagList(
              shiny::modalButton("Keep running"),
              shiny::actionButton("confirm_running_cancel", "Cancel task", class = "btn btn-danger")
            )
          ))
          return(invisible(NULL))
        }

        if (identical(row$status[[1]], "pending")) {
          if (!can_issue_action(paste0("cancel::", task_id))) {
            return(invisible(NULL))
          }

          if (isTRUE(control_via_files)) {
            request_local_cancel(task_id = task_id, row = row)
            request_id <- new_dashboard_session_id()
            remember_pending_cancel(task_id = task_id, request_id = request_id)
            shiny::showNotification("Pending task cancellation requested.", type = "message")
            refresh_nonce(refresh_nonce() + 1L)
          } else if (isTRUE(control_via_server)) {
            request_id <- send_control_request("cancel", task_id = task_id)
            if (!is.null(request_id)) {
              remember_pending_cancel(task_id = task_id, request_id = request_id)
            }
          } else {
            tryCatch(
              {
                cancel_task(task_id)
                shiny::showNotification("Pending task canceled.", type = "message")
              },
              error = function(e) {
                shiny::showNotification(conditionMessage(e), type = "error")
              }
            )
          }
        }
        invisible(NULL)
      }

      remove_card_task <- function(task_id) {
        if (!isTRUE(can_control)) {
          return(invisible(NULL))
        }

        current <- state_tasks()
        row <- current[current$id == task_id, , drop = FALSE]
        if (nrow(row) == 0) {
          return(invisible(NULL))
        }

        if (identical(row$status[[1]], "running")) {
          pending_running_remove(task_id)
          shiny::showModal(shiny::modalDialog(
            title = "Remove running task?",
            sprintf("Task '%s' is running. Remove will cancel it first.", row$card_title[[1]]),
            footer = shiny::tagList(
              shiny::modalButton("Keep running"),
              shiny::actionButton("confirm_running_remove", "Remove task", class = "btn btn-danger")
            )
          ))
          return(invisible(NULL))
        }

        if (!can_issue_action(paste0("remove::", task_id))) {
          return(invisible(NULL))
        }

        if (isTRUE(control_via_server)) {
          send_control_request("remove", task_id = task_id)
        } else if (isTRUE(control_via_files)) {
          shiny::showNotification("Single-task removal requires the dashboard control server.", type = "error")
        } else {
          tryCatch(
            {
              remove_task(task_id)
              shiny::showNotification("Task removed.", type = "message")
            },
            error = function(e) {
              shiny::showNotification(conditionMessage(e), type = "error")
            }
          )
        }
        refresh_nonce(refresh_nonce() + 1L)
        invisible(NULL)
      }

      shiny::observeEvent(input$taskr_card_action, {
        event <- input$taskr_card_action
        action <- as.character(event$action %||% "")
        task_id <- tryCatch(normalize_task_id(event$task_id), error = function(e) NA_integer_)
        if (!nzchar(action) || is.na(task_id)) {
          return(invisible(NULL))
        }

        if (identical(action, "select")) {
          return(select_card_task(task_id))
        }
        if (identical(action, "cancel")) {
          return(cancel_card_task(task_id))
        }
        if (identical(action, "remove")) {
          return(remove_card_task(task_id))
        }
        invisible(NULL)
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$confirm_running_cancel, {
        if (!isTRUE(can_control)) {
          return(invisible(NULL))
        }
        task_id <- pending_running_cancel()
        close_dashboard_modal()
        if (is.null(task_id)) {
          return(invisible(NULL))
        }

        if (!can_issue_action(paste0("cancel::", task_id))) {
          pending_running_cancel(NULL)
          return(invisible(NULL))
        }

        if (isTRUE(control_via_files)) {
          current <- state_tasks()
          row <- current[current$id == task_id, , drop = FALSE]
          request_local_cancel(task_id = task_id, row = row)
          request_id <- new_dashboard_session_id()
          remember_pending_cancel(task_id = task_id, request_id = request_id)
          shiny::showNotification("Running task cancellation requested.", type = "message")
          refresh_nonce(refresh_nonce() + 1L)
        } else if (isTRUE(control_via_server)) {
          request_id <- send_control_request("cancel", task_id = task_id)
          if (!is.null(request_id)) {
            remember_pending_cancel(task_id = task_id, request_id = request_id)
          }
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

      shiny::observeEvent(input$confirm_running_remove, {
        if (!isTRUE(can_control)) {
          return(invisible(NULL))
        }
        task_id <- pending_running_remove()
        close_dashboard_modal()
        if (is.null(task_id)) {
          return(invisible(NULL))
        }

        if (!can_issue_action(paste0("remove::", task_id))) {
          pending_running_remove(NULL)
          return(invisible(NULL))
        }

        if (isTRUE(control_via_server)) {
          send_control_request("remove", task_id = task_id)
        } else if (isTRUE(control_via_files)) {
          shiny::showNotification("Single-task removal requires the dashboard control server.", type = "error")
        } else {
          tryCatch(
            {
              remove_task(task_id)
              shiny::showNotification("Task removed.", type = "message")
            },
            error = function(e) {
              shiny::showNotification(conditionMessage(e), type = "error")
            }
          )
        }
        pending_running_remove(NULL)
        refresh_nonce(refresh_nonce() + 1L)
        invisible(NULL)
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$clean_finished, {
        if (!isTRUE(can_control)) {
          return(invisible(NULL))
        }

        if (isTRUE(control_via_server)) {
          send_control_request("clean_finished")
          return(invisible(NULL))
        }

        if (isTRUE(control_via_files)) {
          request_local_clean_finished()
          refresh_nonce(refresh_nonce() + 1L)
          shiny::showNotification("Finished task cleanup requested.", type = "message")
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
          "This will cancel all running and pending tasks, and remove finished task records.",
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

        close_dashboard_modal()
        if (!can_issue_action("clear_all")) {
          return(invisible(NULL))
        }

        if (isTRUE(control_via_files)) {
          all_tab <- state_tasks()
          active <- all_tab[all_tab$status %in% c("running", "pending"), , drop = FALSE]
          if (nrow(active) > 0) {
            for (i in seq_len(nrow(active))) {
              task_id <- as.character(active$id[[i]])
              request_local_cancel(task_id = task_id, row = active[i, , drop = FALSE])
            }
            pending_cancel_ids(unique(c(pending_cancel_ids(), active$id)))
          }
          request_local_clean_finished()
          refresh_nonce(refresh_nonce() + 1L)
          shiny::showNotification("All task cleanup requested.", type = "message")
        } else if (isTRUE(control_via_server)) {
          all_tab <- state_tasks()
          active_ids <- all_tab$id[all_tab$status %in% c("running", "pending")]
          if (length(active_ids) > 0) {
            pending_cancel_ids(unique(c(pending_cancel_ids(), active_ids)))
          }
          send_control_request("clear_all")
        } else {
          slots <- as.integer(pkg_env$scheduler$capacity$slots %||% 1L)
          if (is.na(slots) || slots < 1L) {
            slots <- 1L
          }

          tryCatch(
            {
              shutdown_queue()
              init_queue(max_slots = slots)
              drop_stale_display_progress(character())
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
        active_ids <- all_tab$id[all_tab$status %in% c("running", "pending")]
        active_keys <- as.character(active_ids)
        pending_cancel_ids(intersect(pending_cancel_ids(), active_keys))

        req <- pending_cancel_requests()
        if (length(req) > 0) {
          keep <- as.character(unname(req)) %in% active_keys
          pending_cancel_requests(req[keep])
        }
      })

      shiny::observeEvent(input$taskr_control_result, {
        result <- input$taskr_control_result
        request_id <- as.character(result$request_id %||% "")
        action <- as.character(result$action %||% "")

        if (identical(action, "cancel") && nzchar(request_id)) {
          forget_pending_cancel(request_id)
        }
        if (identical(action, "clear_all")) {
          pending_cancel_ids(character())
          pending_cancel_requests(setNames(character(), character()))
        }

        ok <- isTRUE(result$ok)
        message <- as.character(result$message %||% "")
        error <- as.character(result$error %||% "")
        if (ok) {
          shiny::showNotification(if (nzchar(message)) message else "Dashboard control action completed.", type = "message")
        } else {
          shiny::showNotification(if (nzchar(error)) error else "Dashboard control action failed.", type = "error")
        }

        refresh_nonce(refresh_nonce() + 1L)
        invisible(NULL)
      }, ignoreInit = TRUE)

      shiny::observe({
        # Keep expanded id valid when data updates.
        current_id <- selected_id()
        if (is.null(current_id)) return(invisible(NULL))

        visible_ids <- unique(c(
          filtered_running_tasks()$id,
          state_split()$pending$id,
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
          last_focus_target("")
          return(invisible(NULL))
        }

        running_ids <- filtered_running_tasks()$id
        pending_ids <- state_split()$pending$id
        finished_all <- state_split()$finished
        selected_filter <- finished_filter_status()
        finished_filtered_ids <- finished_all$id[finished_all$status == selected_filter]

        container_key <- NULL
        if (current_id %in% running_ids) {
          container_key <- "col-running"
        } else if (current_id %in% pending_ids) {
          container_key <- "col-pending"
        } else if (current_id %in% finished_filtered_ids) {
          container_key <- "col-finished"
        }

        if (is.null(container_key)) {
          return(invisible(NULL))
        }

        focus_target <- paste(current_id, container_key, sep = "::")
        if (identical(last_focus_target(), focus_target)) {
          return(invisible(NULL))
        }
        last_focus_target(focus_target)

        session$onFlushed(function() {
          session$sendCustomMessage(
            "taskr_focus_task",
            list(task_id = current_id, container_key = container_key)
          )
        }, once = TRUE)
      })

      shiny::observe({
        payload <- expanded_running_log_text()
        if (is.null(payload)) {
          return(invisible(NULL))
        }
        session$sendCustomMessage(
          "taskr_update_logs",
          list(task_id = payload$task_id, text = payload$text)
        )
        invisible(NULL)
      })

    }
  )

  app
}

#' Open the Shiny Queue Dashboard
#'
#' Purpose:
#' - Open a visual monitoring dashboard for pending/running/terminal tasks.
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
