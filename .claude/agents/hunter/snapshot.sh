# Where a hunter reads and writes: a snapshot of the tree, `.claude/snapshots/<commit>/`, its compiler and its board in `bin/`, and a directory for each hunter in `hunters/hunter-<n>/`. Sourced by `guard.sh` and `write-guard.sh`, which judge every path by these tests. A path is resolved before it is judged, so `..` and a link reach nothing the tests would refuse spelled out.
resolve() {
    python3 -c 'import os, sys; print(os.path.realpath(sys.argv[1]))' "$1"
}
pool=$(resolve "$CLAUDE_PROJECT_DIR/.claude/snapshots")

# What in a snapshot `$1` resolves to, past the snapshot's own directory, and fail where it resolves outside every snapshot.
within_snapshot() {
    local resolved inner commit
    resolved=$(resolve "$1")
    inner=${resolved#"$pool"/}
    if [[ $inner == "$resolved" ]]; then
        return 1
    fi
    commit=${inner%%/*}
    if [[ ! $commit =~ ^[0-9a-f]+$ || $inner == "$commit" ]]; then
        return 1
    fi
    printf '%s\n' "${inner#"$commit"/}"
}

# Which of a snapshot's two binaries `$1` is, `curios` or `xboard`, and fail where it is neither.
snapshot_binary() {
    local rest
    rest=$(within_snapshot "$1") || return 1
    case $rest in
        bin/curios | bin/xboard) printf '%s\n' "${rest#bin/}" ;;
        *) return 1 ;;
    esac
}

# Whether `$1` resolves to a file inside a hunter's directory of a snapshot, `hunters/hunter-<n>/`.
within_hunter_directory() {
    local rest name
    rest=$(within_snapshot "$1") || return 1
    if [[ $rest != hunters/* ]]; then
        return 1
    fi
    rest=${rest#hunters/}
    name=${rest%%/*}
    [[ $name =~ ^hunter-[0-9]+$ && $rest == "$name"/?* ]]
}
