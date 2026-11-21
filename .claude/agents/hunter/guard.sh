#!/usr/bin/env bash
# The hunter's shell: the PreToolUse hook of `.claude/agents/hunter.md`. It asks `judge.sh` beside it whether the command is one a hunt runs, and hands back what the judge admits to run inside a box: under bash, as it was judged, in a world the kernel holds read-only but for the hunters' directories of the snapshots, with no network. The judge reads words and cannot see what a program does; that nothing is written outside a hunter's directory, whatever a program does, is the box's. Anything it cannot box it refuses.
set -euo pipefail
verdict=''
trap 'case $verdict in admitted) exit 0 ;; refused) exit 2 ;; *) echo "hunter guard: could not box the command, so it is refused" >&2; exit 2 ;; esac' EXIT
here=$(dirname "${BASH_SOURCE[0]}")
source "$here/snapshot.sh"

input=$(cat)
if ! "$here/judge.sh" <<<"$input"; then
    verdict=refused
    exit 2
fi
command=$(jq -r '.tool_input.command // empty' <<<"$input")
if [[ -z $command ]]; then
    verdict=admitted
    exit 0
fi
if ! command -v bwrap >/dev/null; then
    echo 'hunter guard: refused: a hunter runs inside bubblewrap, and `bwrap` is not installed.' >&2
    verdict=refused
    exit 2
fi

# One word of the command line the harness evaluates, whatever it holds.
quoted() {
    printf "'%s'" "${1//\'/\'\\\'\'}"
}

# The whole filesystem read-only, a `/tmp` of its own, each snapshot's `hunters/` writable, no network, and the command gone with the hook's caller.
boxed='bwrap --die-with-parent --new-session --unshare-net --ro-bind / / --dev /dev --proc /proc --tmpfs /tmp'
for hunters in "$pool"/*/hunters; do
    if [[ -d $hunters ]]; then
        boxed+=" --bind $(quoted "$hunters") $(quoted "$hunters")"
    fi
done
boxed+=" -- bash -c $(quoted "$command")"

# The tool's whole input, its command replaced: the harness runs what the hook hands back.
jq -c --arg command "$boxed" '{hookSpecificOutput: {hookEventName: "PreToolUse", updatedInput: (.tool_input + {command: $command})}}' <<<"$input"
verdict=admitted
