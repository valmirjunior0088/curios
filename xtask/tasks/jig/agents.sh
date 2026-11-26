#!/bin/sh
# What the agent files hold of the tree: each glob of a rule matches a tracked file, and each path and source name they cite in a code span is in the tree. A rule's globs decide where it loads and a citation is where a session is sent; each is text no compiler reads, so a moved file fails nothing until it is put to the tree here. Their links are every Markdown file's, which `xtask/tasks/docs/links.sh` holds. Every departure is reported, a line each, and the step fails where there is one.
#
# Arguments, as `jig agents` hands them over: the agent files, then `--`, then the patterns of the files that hold the sources' names.
set -eu

cd "$(git rev-parse --show-toplevel)"

wrong=$(mktemp)
places=$(mktemp)
agent_files=$(mktemp)
trap 'rm -f "$wrong" "$places" "$agent_files"' EXIT

while [ "$#" -gt 0 ] && [ "$1" != -- ]; do
    printf '%s\n' "$1" >> "$agent_files"
    shift
done
[ "$#" -gt 0 ] && shift

tracked() {
    git ls-files
}

# Every tracked file and every directory holding one, a line each: what a citation may lead to.
tracked | awk -F/ '{ path = $1; print path; for (i = 2; i <= NF; i++) { path = path "/" $i; print path } }' | sort -u > "$places"

# Whether `$1` is a tracked file or a directory of tracked files.
held() {
    grep -qxF -- "$1" "$places"
}

# The globs a rule's front matter lists under `paths:`, one a line.
globs() {
    awk '
        NR == 1 { if ($0 != "---") exit; matter = 1; next }
        matter && $0 == "---" { exit }
        matter && $0 == "paths:" { listing = 1; next }
        listing && /^[ \t]*- / { sub(/^[ \t]*- /, ""); gsub(/"/, ""); print; next }
        listing { listing = 0 }
    ' "$1"
}

# Every code span of a file past its front matter and outside its fenced blocks, a line each.
spans() {
    awk '
        NR == 1 && $0 == "---" { matter = 1; next }
        matter { if ($0 == "---") matter = 0; next }
        /^[ \t]*```/ { fenced = !fenced; next }
        fenced { next }
        {
            rest = $0
            while ((start = index(rest, "`")) > 0) {
                run = 0
                while (substr(rest, start + run, 1) == "`") run++
                fence = substr(rest, start, run)
                inside = substr(rest, start + run)
                found = 0
                from = 1
                while ((at = index(substr(inside, from), fence)) > 0) {
                    at += from - 1
                    before = (at > 1) ? substr(inside, at - 1, 1) : ""
                    after = substr(inside, at + run, 1)
                    if (before != "`" && after != "`") { found = at; break }
                    from = at + 1
                }
                if (!found) break
                span = substr(inside, 1, found - 1)
                gsub(/^[ \t]+|[ \t]+$/, "", span)
                print span
                rest = substr(inside, found + run)
            }
        }
    ' "$1"
}

top=$(tracked | awk -F/ '{ print $1 }' | sort -u)

while IFS= read -r file; do
    globs "$file" | while IFS= read -r glob; do
        if [ -z "$(git ls-files -- ":(glob)$glob" | head -n 1)" ]; then
            echo "$file: the glob \`$glob\` matches no tracked file" >> "$wrong"
        fi
    done

    spans "$file" | while IFS= read -r text; do
        case $text in
            *[[:space:]]* | *[\<\>*{}\$\|]* | *…*) continue ;;
        esac
        # An identifier of the sources: lowercase words joined by underscores.
        if printf '%s\n' "$text" | grep -Eq '^[a-z][a-z0-9]*(_[a-z0-9]+)+$'; then
            if ! git grep -q -w -F -- "$text" -- "$@"; then
                echo "$file: no source holds the name \`$text\`" >> "$wrong"
            fi
            continue
        fi
        # A path of the repository starts at one of its top-level entries: `/std/Str` is a path of the language, and `origin/main` a branch.
        path=${text%%:*}
        path=${path%/}
        first=${path%%/*}
        case $text in */*) ;; *) continue ;; esac
        printf '%s\n' "$top" | grep -qxF -- "$first" || continue
        held "$path" && continue
        # A directory the ignore file names is asked for by a file inside it: one a build has not made yet is no directory to git.
        if ! git check-ignore "$path" "$path/-" > /dev/null; then
            echo "$file: the path \`$text\` is not in the tree" >> "$wrong"
        fi
    done
done < "$agent_files"

if [ -s "$wrong" ]; then
    cat "$wrong" >&2
    exit 1
fi
