---
name: address-comments
description: Address unresolved review comments on the current pull request, asking the user to choose when multiple valid resolutions exist, then validate, commit, push, and resolve the comments that were fixed. Use only when the user explicitly invokes $address-comments.
---

# Address Comments

## Overview

Review unresolved pull request feedback, implement the selected fixes, verify the result with tests and linting where available, and then commit, push, and resolve the comments that were addressed.

## Workflow

1. Inspect the current pull request and gather the unresolved review comments or requested changes.
2. Group comments by file and theme so related fixes can be handled together.
3. If a comment can be addressed in multiple materially different ways, stop and ask the user which approach to take before editing.
4. Implement the agreed fixes without disturbing unrelated work.
5. Run the relevant test and lint commands for the affected area. If there is no obvious command, inspect the repository tooling and choose the appropriate validation commands.
6. Commit the changes with a focused commit message and push the branch.
7. Resolve only the comments that were actually addressed.

## Decision Rules

- Ask the user before choosing between competing valid interpretations of review feedback.
- Do not resolve a thread unless the underlying concern was addressed in code or explicitly deferred with user approval.
- If validation fails, fix the issue before committing, or stop and report the blocker if it cannot be resolved safely.

## GitHub Notes

- Prefer GitHub-aware tools and workflows when available for comment inspection and thread resolution.
- Work from unresolved comments only; avoid reopening resolved review history unless it is directly relevant.
