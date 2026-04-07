---
description: Commit the current work
agent: build
---

Review the current worktree before committing anything.

Create one or more atomic commits from the current changes. Split unrelated work
into separate commits whenever feasible so the result is easy to review and
revert.

If the branch name contains a ticket identifier then begin the commit summary
with that identifier.

If there is no ticket identifier available then use an appropriate
conventional-commit prefix such as `feat:`, `fix:`, `refactor:`, `docs:`, or
`chore:`.

Stage only the files that belong in the current atomic commit. Do not create
empty commits.

Push after committing if and only if the current branch is not `develop`,
`main`, or `master`.
