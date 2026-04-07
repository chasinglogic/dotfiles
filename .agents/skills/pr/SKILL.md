---
name: pr
description: Create a GitHub pull request for the current branch targeting the repository default branch, using a ticket-based title when the branch includes a ticket identifier and a description that explains what changed and why. Use only when the user explicitly invokes $pr.
---

# Pr

## Overview

Prepare and open a pull request from the current branch to the repository default branch. Derive the title from the ticket when available and write a concise changelog-style description that explains what changed and why.

## Workflow

1. Determine the current branch and the repository default branch.
2. Confirm the branch is pushed and contains the intended changes for review.
3. If the branch name includes a ticket identifier, use that ticket number and summary in the pull request title.
4. If no ticket identifier is available, derive a clear title from the branch purpose or recent commit history.
5. Write the pull request body as a compact changelog that explains what changed and why.
6. Create the pull request against the default branch using GitHub-aware tooling when available.

## Title Rules

- Prefer `TICKET-123 Summary of ticket` when the branch includes a ticket identifier and the ticket summary is available.
- Otherwise, use a concise human-readable title that matches the actual change.

## Body Rules

- Include a short changelog of what was done.
- Include the reason for the change, not just the file list.
- Keep the body easy for reviewers to scan.
