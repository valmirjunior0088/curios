#!/usr/bin/env bash
# The hunter's writes: the PreToolUse hook of `.claude/agents/hunter.md` for `Write` and `Edit`, admitting a file only inside a hunter's directory of a snapshot, as `snapshot.sh` beside it judges one.
set -euo pipefail
verdict=''
trap 'case $verdict in admitted) exit 0 ;; refused) exit 2 ;; *) echo "hunter write guard: could not judge the write, so it is refused" >&2; exit 2 ;; esac' EXIT
source "$(dirname "${BASH_SOURCE[0]}")/snapshot.sh"

path=$(jq -r '.tool_input.file_path // empty')
if [[ -z $path ]] || ! within_hunter_directory "$path"; then
    printf 'hunter write guard: refused a write to `%s`: a hunter writes only inside its directory of the snapshot, `.claude/snapshots/<commit>/hunters/hunter-<n>/`.\n' "$path" >&2
    verdict=refused
    exit 2
fi
verdict=admitted
