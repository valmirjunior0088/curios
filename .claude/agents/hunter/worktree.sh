# Where a hunter may write, build and revert: inside a worktree of its own, `.claude/worktrees/hunter-<n>`, and never that worktree's `.claude/`, `CLAUDE.md` or `.git`, which hold its rules and its history. Sourced by `guard.sh` and `write-guard.sh`, which judge every path by this one test. A path is resolved before it is judged, so `..` and a link reach nothing the test would refuse spelled out.
pool=$(realpath -m -- "$CLAUDE_PROJECT_DIR/.claude/worktrees")

# Print where in its worktree `$1` resolves, empty for the worktree's root, and fail where it resolves outside every hunter's worktree or into what a hunter may not touch.
within_worktree() {
    local resolved inner name rest
    resolved=$(realpath -m -- "$1")
    inner=${resolved#"$pool"/}
    if [[ $inner == "$resolved" ]]; then
        return 1
    fi
    name=${inner%%/*}
    rest=${inner#"$name"}
    rest=${rest#/}
    if [[ ! $name =~ ^hunter-[0-9]+$ ]]; then
        return 1
    fi
    case $rest in
        .claude | .claude/* | .git | .git/* | CLAUDE.md) return 1 ;;
    esac
    printf '%s\n' "$rest"
}
