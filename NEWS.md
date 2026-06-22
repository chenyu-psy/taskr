# taskr 0.3.0

- Rewrote `README.md` around a dashboard-first user workflow and refreshed the
  dashboard screenshots.
- Switched task identity to simple numeric ids. Public task operations now use
  task ids instead of labels; labels remain display and filtering metadata.
- Renamed waiting task status from `queued` to `pending`.
- Updated `cancel_task()`, `remove_task()`, and `get_task_overview(id = ...)`
  to accept vector task ids.
- Compact dashboard task cards now show titles like `1: model_exp4`, use the
  top-right badge for pending priority and queue position, elapsed time,
  duration, or cancellation state, and remove the separate id/duration rows.
- Added `remove_task()` to remove one or more pending, running, completed,
  failed, or cancelled tasks by id.
- Added dashboard remove controls for individual task cards, including a
  confirmation step before removing a running task.
- Added a dashboard control-server `/remove` action so background dashboards can
  remove one task without clearing all finished records.
- Refactored dashboard task cards to persist in the browser and update logs in
  place, avoiding full card-list refreshes that could reset expanded console
  scroll position.
- Fixed task completion semantics so `output = "none"` tasks are completed when
  the submitted expression or function returns normally, including
  brms/cmdstanr fits that save model files outside taskr.
- Improved failed-task messages by keeping cached stderr available after queue
  polling.

# taskr 0.0.1

- Initial package scaffold with one exported example function.
- Added testthat test setup.
- Added GitHub Actions workflow for release on `main`.
