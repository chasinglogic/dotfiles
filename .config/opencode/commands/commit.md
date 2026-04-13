---
description: Commit the current work
agent: build
---

# Commit

## Overview

Review the current worktree, group changes into the smallest reasonable atomic commits, and write commit messages that match the branch naming rules. Push after committing unless the current branch is `develop`, `main`, or `master`.

## Workflow

1. Inspect the repository state with `git status --short --branch` and review the staged and unstaged changes before committing anything.
2. Determine whether the branch name contains a ticket identifier. If it does, put that identifier at the start of the commit summary.
3. If no ticket identifier is available, use a conventional commit prefix such as `feat:`, `fix:`, `refactor:`, `docs:`, or `chore:`.
4. Split unrelated changes into separate commits when feasible. Prefer small, reversible commits over one large commit.
5. Stage only the files that belong in the current atomic commit.
6. Write a concise summary line. Add a body only when it materially improves clarity.
7. After committing, push the branch if and only if the current branch is not `develop`, `main`, or `master`.

## Commit Message Rules

- When the branch contains a recognizable ticket like `ABC-123`, format the summary to begin with that identifier.
- When no ticket is available, use conventional commits.
- Keep the summary specific to the atomic change being committed.

## Safety Rules

- Do not include unrelated changes in the same commit.
- Do not create empty commits.
- Do not push protected integration branches: `develop`, `main`, `master` unless the user specifically says push at the bottom of this prompt.
- If the worktree contains unrelated user changes that cannot be cleanly separated, stop and ask before proceeding.
