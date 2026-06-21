# Repository Instructions

## Planning Records

- Keep `Plan.md` synchronized with future repository changes.
- Before making code, documentation, workflow, release, or UI changes, check
  whether the change affects a milestone, milestone status, exit criteria, or
  deferred scope in `Plan.md`.
- If it does, update `Plan.md` in the same work session as the change.
- Keep `Plan.md` in English only.
- Keep `Plan.md` milestone-based. Do not add detailed implementation logs,
  transient debugging notes, or redundant changelog entries there.

## Release Notes

- For release-facing changes, update `NEWS.md` under the current package
  version.
- Keep `DESCRIPTION` version and the top `NEWS.md` heading aligned before
  merging to `main`.

## Git and Pull Requests

- Run git operations outside the sandbox when they need repository index, ref,
  remote, or authentication access.
- Feature and fix branches must open pull requests into `develop` first.
- Only open a pull request from `develop` into `main` after the `develop`
  branch has been reviewed and checked.
- Before opening a pull request into `main`, verify release prerequisites:
  `DESCRIPTION` version is bumped as needed, the top `NEWS.md` heading matches
  that version, release notes are updated, generated documentation is current,
  and relevant tests/checks have passed.
- If any `main` pull-request prerequisite is missing or uncertain, stop before
  creating the PR and tell the user what must be completed first.
