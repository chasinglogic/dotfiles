---
description: Multi-lens code review
agent: build
---

Run `git fetch` first so the remote tracking branches are current.

Determine the review base branch:

- Use `origin/develop` if that remote branch exists
- Otherwise use `origin/main`

Analyse the current branch diff with `git diff --stat <base>...HEAD` and
`git diff <base>...HEAD`.

Classify the changed files by review lens:

- @review-frontend for any .tsx, .css, .scss files
- @review-backend for any .go files, .py files, .ts files in api/ or server/
- @review-infra for any config, yaml, terraform, or deployment files

Invoke the applicable reviewers in parallel using the OpenCode syntax above.

Use these review priorities for each lens:

- @review-frontend: React component patterns, accessibility, performance,
  CSS architecture, user experience, and missing test coverage for changed
  user-facing behavior
- @review-backend: API design, database queries including N+1 problems,
  error handling, security, and missing tests around changed behavior and edge
  cases
- @review-infra: deployment configuration, environment variables, health
  checks, monitoring and observability, cloud cost, and operational safety

Synthesise the findings into a single prioritised report ordered by severity.
Present findings first with concrete file references when possible. Keep the
summary brief and secondary to the findings. If no meaningful issues are found,
say so explicitly and mention any residual risks or testing gaps.
