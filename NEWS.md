# taskr 0.2.2

- Added `remove_task()` to remove one queued, running, completed, failed, or
  cancelled task by id or label.
- Added dashboard remove controls for individual task cards, including a
  confirmation step before removing a running task.
- Added a dashboard control-server `/remove` action so background dashboards can
  remove one task without clearing all finished records.
- Fixed `output = "none"` task completion when user code saves results outside
  taskr, such as Stan/CmdStan model fits written to local files.
- Improved failed-task messages by keeping cached stderr available after queue
  polling.

# taskr 0.0.1

- Initial package scaffold with one exported example function.
- Added testthat test setup.
- Added GitHub Actions workflow for release on `main`.
