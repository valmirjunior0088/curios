#!/usr/bin/env bash
# The hunter's writes: the PreToolUse hook of `.claude/agents/hunter.md` for `Write` and `Edit`, admitting a file only inside a hunter's worktree, as `worktree.sh` beside it judges one.
set -euo pipefail
trap 'echo "hunter write guard: could not judge the write, so it is refused" >&2; exit 2' ERR
source "$(dirname "${BASH_SOURCE[0]}")/worktree.sh"

path=$(jq -r '.tool_input.file_path // empty')
if [[ -z $path ]] || ! within_worktree "$path" >/dev/null; then
    printf 'hunter write guard: refused a write to `%s`: a hunter writes only inside its worktree, `.claude/worktrees/hunter-<n>`, and never its `.claude/`, `CLAUDE.md` or `.git`.\n' "$path" >&2
    exit 2
fi
