---
name: review
description: Review the current branch diff against the repository integration branch, preferring `origin/develop` when it exists and otherwise using `origin/main`, then examine the changed files through frontend, backend, and infrastructure lenses as applicable and synthesize findings into one prioritized report. Use only when the user explicitly invokes $review to run a manual code review.
---

# Review

## Overview

Run `git fetch` first so the remote tracking branches are current. Then determine the review base branch: use `origin/develop` when it exists, otherwise use `origin/main`. Analyze `git diff <base>...HEAD`, decide which review lenses apply based on the changed files, and produce a concise prioritized review report. Focus on bugs, regressions, risks, and missing tests rather than style nits.

Use `Use $review in parallel` or equivalent wording such as `with subagents` to explicitly request parallel lens reviewers.

Load the relevant lens reference files before reviewing:
- `references/review-frontend.md`
- `references/review-backend.md`
- `references/review-infra.md`

## Workflow

1. Run `git fetch` before reviewing so the remote base branches are up to date.
2. Determine the diff base:
   - Use `origin/develop` if that remote branch exists.
   - Otherwise use `origin/main`.
3. Start with `git diff --stat <base>...HEAD` and `git diff <base>...HEAD` to understand scope and details.
4. Classify the changed files by lens:
   - Frontend: `.tsx`, `.css`, `.scss`
   - Backend: `.go`, `.py`, and `.ts` files under `api/` or `server/`
   - Infra: config files, YAML, Terraform, deployment-related files
5. Load the lens reference files that match the changed files.
6. Review every relevant file with the applicable lens or lenses, using the loaded reviewer prompts as the standard even in single-agent mode.
7. Synthesize the results into one prioritized report ordered by severity.
8. If no meaningful issues are found, state that explicitly and call out residual risks or testing gaps.

## Parallel Mode

- When the user explicitly requests `in parallel`, `with subagents`, or similar wording, spawn separate reviewer subagents for each applicable lens and run them concurrently.
- Give each subagent the matching reviewer reference file so the delegated review uses the same lens prompt as single-agent review.
- Use the frontend reviewer for `.tsx`, `.css`, and `.scss` changes.
- Use the backend reviewer for `.go`, `.py`, and server-side `.ts` changes.
- Use the infra reviewer for config, YAML, Terraform, and deployment-related changes.
- After the subagents complete, synthesize their findings into one prioritized report and remove duplication.
- If the user does not explicitly request parallel review, perform the review in a single agent.

## Lens Files

- Frontend lens: `references/review-frontend.md`
- Backend lens: `references/review-backend.md`
- Infra lens: `references/review-infra.md`

When multiple lenses apply to the same file or change, merge the findings and avoid duplicate reporting.

## Reporting Format

- Present findings first, ordered by severity, with concrete file references when possible.
- Keep the summary brief and secondary to the findings.
- Do not pad the report with style commentary if no functional risk exists.
