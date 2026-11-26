#!/bin/sh
# The grammars `editors/zed/extension.toml` pins held to the grammars this tree holds: each pair of arguments is a pin's `rev` and `path`, the grammar's directory in the repository. Zed installs a grammar from a pushed commit rather than from the checkout, so a pin is a copy of that directory, the tree it publishes at `rev:path` held to the tree the same directory holds on disk, and every pin that departs is reported, a line each.
set -eu

cd "$(git rev-parse --show-toplevel)"

scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
departed=0

while [ "$#" -ge 2 ]; do
    rev=$1
    path=$2
    shift 2

    # A pin names its directory as its revision holds it, so a directory moved since is moved in the pin with the revision, once the move is pushed.
    if [ ! -d "$path" ]; then
        echo "extension.toml pins $rev:$path, a directory this tree no longer holds: push the tree, then move the rev and the path" >&2
        departed=1
        continue
    fi

    # The tree the directory holds on disk, as a commit of this checkout would publish it: a working tree has no tree object until something writes one, so it is written through an index of its own, seeded from `HEAD`. Held to the working tree and not to `HEAD`, a grammar change is caught before it is committed.
    index=$scratch/index
    GIT_INDEX_FILE=$index git read-tree HEAD
    GIT_INDEX_FILE=$index git add -A -- "$path"
    held=$(GIT_INDEX_FILE=$index git write-tree --prefix="$path")
    rm -f "$index"

    # A working checkout holds the pinned commit whenever it is an ancestor, so the common case resolves offline; a checkout one commit deep, as the check workflow's is, fetches it.
    if ! published=$(git rev-parse --verify --quiet "$rev:$path"); then
        git fetch --quiet --depth=1 origin "$rev"
        if ! published=$(git rev-parse --verify --quiet "$rev:$path"); then
            echo "extension.toml pins $rev:$path, which origin does not hold" >&2
            departed=1
            continue
        fi
    fi

    if [ "$published" != "$held" ]; then
        echo "extension.toml pins $rev:$path, publishing grammar tree $published, but this tree holds $held there: commit and push $path, then move the rev" >&2
        departed=1
    fi
done

exit "$departed"
