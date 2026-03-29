---
description: Multi-lens code review
agent: build
---

Analyse the current diff with `git diff main...HEAD`.

Then invoke these reviewers in parallel:

- @review-frontend for any .tsx, .css, .scss files
- @review-backend for any .go files, .py files, .ts files in api/ or server/
- @review-infra for any config, yaml, terraform, or deployment files

Synthesise their findings into a single prioritised report.
