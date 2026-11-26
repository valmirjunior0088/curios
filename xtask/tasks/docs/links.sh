#!/bin/sh
# Every link a Markdown file of the tree writes leads somewhere: a relative link to a tracked file or a directory of them, and an anchor to a heading of the file it names, read off its headings as a renderer makes their anchors. A link is text no compiler reads, so a moved file or a renamed heading fails nothing until it is put to the tree here. Every departure is reported, a line each, and the step fails where there is one.
#
# Arguments, as `docs links` hands them over: the Markdown files.
set -eu

cd "$(git rev-parse --show-toplevel)"

wrong=$(mktemp)
places=$(mktemp)
trap 'rm -f "$wrong" "$places"' EXIT

# Every tracked file and every directory holding one, a line each: what a link may lead to.
git ls-files | awk -F/ '{ path = $1; print path; for (i = 2; i <= NF; i++) { path = path "/" $i; print path } }' | sort -u > "$places"

# Whether `$1` is a tracked file or a directory of tracked files.
held() {
    grep -qxF -- "$1" "$places"
}

# Every link target of a file past its front matter and outside its fenced blocks and its code spans, a line each.
links() {
    awk '
        NR == 1 && $0 == "---" { matter = 1; next }
        matter { if ($0 == "---") matter = 0; next }
        /^[ \t]*```/ { fenced = !fenced; next }
        fenced { next }
        {
            line = $0
            gsub(/`[^`]*`/, "", line)
            while ((at = index(line, "](")) > 0) {
                line = substr(line, at + 2)
                shut = index(line, ")")
                if (shut == 0) break
                target = substr(line, 1, shut - 1)
                if (target != "" && index(target, "://") == 0 && index(target, " ") == 0 && index(target, "mailto:") != 1) print target
            }
        }
    ' "$1"
}

# The anchors a renderer makes of a file's headings, a line each: a heading lowercased, its punctuation dropped but for `-` and `_`, its spaces made `-`, and a repeated one numbered.
anchors() {
    awk '
        /^[ \t]*```/ { fenced = !fenced; next }
        fenced { next }
        /^#+[ \t]/ {
            heading = $0
            sub(/^#+[ \t]+/, "", heading)
            sub(/[ \t]+#*[ \t]*$/, "", heading)
            gsub(/\]\([^)]*\)/, "", heading)
            heading = tolower(heading)
            gsub(/[^[:alnum:] _-]/, "", heading)
            gsub(/ /, "-", heading)
            anchor = heading
            if (seen[heading]++) anchor = heading "-" (seen[heading] - 1)
            print anchor
        }
    ' "$1"
}

# `$2` without its `.` and `..` segments, as a link from the directory `$1` reads it.
resolved() {
    printf '%s/%s\n' "$1" "$2" | awk -F/ '{
        n = 0
        for (i = 1; i <= NF; i++) {
            if ($i == "" || $i == ".") continue
            if ($i == "..") { if (n > 0) n--; continue }
            kept[++n] = $i
        }
        out = ""
        for (i = 1; i <= n; i++) out = out (i > 1 ? "/" : "") kept[i]
        print out
    }'
}

for file in "$@"; do
    directory=$(dirname "$file")
    links "$file" | while IFS= read -r target; do
        path=${target%%#*}
        anchor=
        case $target in *#*) anchor=${target#*#} ;; esac
        case $path in
            '') named=$file ;;
            *) named=$(resolved "$directory" "$path") ;;
        esac
        # A link within its file names a heading of the file it is written in, which is there.
        if [ -n "$path" ] && ! held "$named"; then
            echo "$file: the link \`$target\` leads nowhere" >> "$wrong"
            continue
        fi
        case $named in *.md) ;; *) continue ;; esac
        if [ -n "$anchor" ] && ! anchors "$named" | grep -qxF -- "$anchor"; then
            echo "$file: the link \`$target\` names no heading of \`$named\`" >> "$wrong"
        fi
    done
done

if [ -s "$wrong" ]; then
    cat "$wrong" >&2
    exit 1
fi
