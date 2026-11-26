#!/bin/sh
# The release dance: the workspace's version, the lock file beside it, the bundled standard library's compiler version, one commit, one tag, and the push that publishes them, along with whatever else `main` had not published yet.
#
# Invoking it is the intent to publish: `.github/workflows/release.yml` fires on a `release/*` tag and creates the public release, so once every refusal below has passed the dance runs to its end without asking again. Nothing here builds and nothing here undoes: the check workflow has already had its say on the commits being released, and where a step fails its message says what would undo what was done.
#
# Arguments, as `cargo xtask release` hands them over: the bump — `patch`, `minor`, `major` or a version — then the manifests it moves, the workspace's, whose `[workspace.package]` version every crate inherits and whose lock stands beside it, and the standard library's, whose `compiler` follows that version.
set -eu

cd "$(git rev-parse --show-toplevel)"

bump=$1
workspace=$2
library=$3
case $workspace in
    */*) lock=${workspace%/*}/Cargo.lock ;;
    *) lock=Cargo.lock ;;
esac

refuse() {
    echo "$*" >&2
    exit 1
}

# The value the field `$3` of the table `$2` of the file `$1` holds, the empty table being the file's top; fail where the table holds no such field.
field_of() {
    awk -v table="$2" -v field="$3" '
        BEGIN { inside = (table == "") }
        /^\[/ { inside = ($0 == table) }
        inside && index($0, field " = \"") == 1 { value = substr($0, length(field) + 5); sub(/"$/, "", value); print value; found = 1; exit }
        END { if (!found) exit 1 }
    ' "$1"
}

current=$(field_of "$workspace" '[workspace.package]' version) ||
    refuse "$workspace states no [workspace.package] version"
library_compiler=$(field_of "$library" '' compiler) ||
    refuse "$library states no compiler"

# Whether `$1` is a version as every tag of this repository spells one: three numbers that print back as they arrived, so a prerelease, a sign, surrounding space and a leading zero are each refused.
is_version() {
    printf '%s\n' "$1" | grep -Eq '^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$'
}

# Whether the version `$1` is above the version `$2`: sorted field by field, each as a number, it comes last and is not `$2`.
above() {
    [ "$1" != "$2" ] &&
        [ "$(printf '%s\n%s\n' "$2" "$1" | sort -t. -k1,1n -k2,2n -k3,3n | tail -n 1)" = "$1" ]
}

# The version

is_version "$current" ||
    refuse "$workspace states the version \`$current\`, which is no release version; a version is three numbers, \`major.minor.patch\`"
major=${current%%.*}
minor_and_patch=${current#*.}
minor=${minor_and_patch%%.*}
patch=${minor_and_patch#*.}

# A minor bump zeroes the patch and a major bump zeroes both, so the rest of the range being left is abandoned rather than resumed.
next_patch=$major.$minor.$((patch + 1))
next_minor=$major.$((minor + 1)).0
next_major=$((major + 1)).0.0

# A bump that is neither a word nor a version is answered with the three it could have been: a release is not cut from a guess.
case $bump in
    patch) target=$next_patch ;;
    minor) target=$next_minor ;;
    major) target=$next_major ;;
    *)
        is_version "$bump" ||
            refuse "not a release: \`$bump\`; $current is current — patch $next_patch, minor $next_minor, major $next_major"
        target=$bump
        ;;
esac
tag=release/$target

above "$target" "$current" || refuse "$target is not above $current, the version in the manifest"

# What must hold before anything is written

# The compiler and its bundled library move together, so a stale library declaration is refused before either manifest is written.
[ "$library_compiler" = "$current" ] ||
    refuse "$library declares compiler \"$library_compiler\", which must match the workspace version $current before a release"

dirty=$(git status --porcelain)
[ -z "$dirty" ] || refuse "the tree is not clean, and a release must not carry uncommitted work:
$dirty"

branch=$(git rev-parse --abbrev-ref HEAD)
[ "$branch" = main ] || refuse "a release is cut from main, not $branch"

git fetch origin main

# Commits `main` has not published are pushed by the dance, not refused by it: the push carries them with the bump.
unpulled=$(git rev-list --count HEAD..origin/main)
[ "$unpulled" = 0 ] ||
    refuse "origin/main holds $unpulled commit(s) this tree does not, so the push would be refused after the tag was written; pull before cutting a release"

[ -z "$(git tag --list "$tag")" ] || refuse "$tag is already a tag, so that version was cut"

# The bump, read back

# Set the field `$3` of the table `$2` of the file `$1` from `$4` to `$5`, every other byte kept, the empty table being the file's top; fail where the field does not hold `$4` there.
set_field() {
    awk -v table="$2" -v field="$3" -v from="$4" -v to="$5" '
        BEGIN { inside = (table == "") }
        /^\[/ { inside = ($0 == table) }
        inside && $0 == field " = \"" from "\"" { $0 = field " = \"" to "\""; set = 1 }
        { print }
        END { if (!set) exit 1 }
    ' "$1" > "$1.release" && mv "$1.release" "$1"
}

set_field "$workspace" '[workspace.package]' version "$current" "$target" ||
    refuse "$workspace states no [workspace.package] version \"$current\""
set_field "$library" '' compiler "$current" "$target" ||
    refuse "$library states no compiler \"$current\""

cargo update --workspace --offline

# The write is checked by reading it back: exactly the two manifests and the lock changed, and each changed line is the field the release owns there.
changed=$(git diff --name-only | sort | tr '\n' ' ')
moved=$(printf '%s\n' "$workspace" "$lock" "$library" | sort | tr '\n' ' ')
[ "$changed" = "$moved" ] ||
    refuse "the bump changed $changed not $workspace, $lock and $library alone"

# Refuse where a changed line of the files after `$1` is not the field `$1` moved from the current version to the target.
changes_only() {
    field=$1
    shift
    offending=$(git diff -- "$@" | grep -E '^[+-]' | grep -Ev '^(\+\+\+|---) ' |
        grep -vxF -e "+$field = \"$target\"" -e "-$field = \"$current\"" | head -n 1) || true
    [ -z "$offending" ] || refuse "the bump changed a line that is not the $field: $offending"
}

changes_only version "$workspace" "$lock"
changes_only compiler "$library"

# Publishing

git commit "$workspace" "$lock" "$library" -m "Bump the version to $target"

git tag "$tag" ||
    refuse "the bump is committed and not tagged; a reset over a commit whose only content is the bump is what would undo it"

git push origin main ||
    refuse "nothing is published, and the commit and the tag are local; \`git tag -d $tag\` and a reset over the bump are what would undo them"

git push origin "$tag" ||
    refuse "the version is on main and the release is not cut; \`git push origin $tag\` is what completes it"

printf '\ncut %s at %s\n' "$tag" "$(git rev-parse HEAD)"

# The release is cut by now, so a missing `gh` must not end the dance in failure and say otherwise.
gh run list --workflow release.yml --limit 1 ||
    echo "release.yml is building it; \`gh run list --workflow release.yml\` follows it"
