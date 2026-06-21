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
