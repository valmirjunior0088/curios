---
description: Bump the workspace version, commit it, tag the release, and push both
argument-hint: "[patch, minor, major, or an explicit X.Y.Z]"
allowed-tools: Read, Edit, Bash(git:*), Bash(cargo:*), Bash(gh:*)
---

Dance the release `$ARGUMENTS` names.

A release is four local mutations in order — the version in `Cargo.toml`, the lock file beside it, one commit, one tag — and then the push that publishes them. `.github/workflows/release.yml` fires on a `release/*` tag and creates the public GitHub release from it. Invoking this command is the intent to publish, so once the preflight passes the dance runs to the end without asking again.

## The version

`[workspace.package] version` in the root `Cargo.toml` is the only place a release version is written; every crate inherits it and `Cargo.lock` records it once per workspace member. The extensions under `editors/` carry their own versions and are not part of this. Read the current version from `Cargo.toml`, never from a tag.

`$ARGUMENTS` is `patch`, `minor`, `major`, or the version itself. With no argument, ask which — the current version and the three it could become — and wait for the answer. A release is not cut from a guess.

## Preflight

All of these before touching a file. Any one failing stops the command, naming what failed:

- `git status --porcelain` is empty. Uncommitted work is the user's, and a release must not carry it.
- The branch is `main`, and after `git fetch origin main` it agrees with `origin/main`. A bump on a stale main tags a tree nobody has.
- `git tag --list release/<version>` is empty. A tag already there means the version was cut.

The check workflow is not consulted. It runs on every push to `main` and takes a while, so by the time a release is called for it has already had its say on the commits being released; the tag's own workflow builds and publishes without running it again.

## The dance

1. Set `[workspace.package] version` in `Cargo.toml` to the new version — that line and nothing else.
2. `cargo update --workspace --offline`, which rewrites the members' versions in `Cargo.lock`. Workspace-scoped and offline, so no dependency moves under the release.
3. `git diff` and confirm it is that one line plus the lock's member versions, nothing else. Anything further stops the dance.
4. `git commit Cargo.toml Cargo.lock -m "Bump the version to <version>"` — named paths only, the subject every bump in this history carries.
5. `git tag release/<version>` on that commit. Lightweight, as every existing `release/*` tag is.
6. `git push origin main`, then `git push origin release/<version>`.
7. Report the commit, the tag, and the release run from `gh run list --workflow release.yml --limit 1` with its URL.

## If a step fails

Stop where it failed and say what exists. Before the pushes the commit and the tag are local, and undoing them is destructive, so it is the user's to ask for: state what would undo it — `git tag -d release/<version>` and a reset over a commit whose only content is the bump — and wait.

If `git push origin main` is rejected, the tag is still local and nothing was published; report the rejection rather than forcing or rebasing. If main pushed and the tag did not, say so — the version is on `main` and the release has not been cut, and the tag push is what completes it.

After both pushes nothing is undone here. A published release and a pushed tag are removed by the user, through `gh` and `git push --delete`.

## Never

- Write the version anywhere but `Cargo.toml`'s workspace version.
- Hand-edit `Cargo.lock`.
- Cut a release from a dirty tree or a stale `main`.
- Force, rebase or amend anything to make a push go through.
