#!/bin/sh
#
# Install the `curios` CLI:
#
#   curl -fsSL https://github.com/valmirjunior0088/curios/releases/latest/download/install.sh | sh
#
# The version is fixed at release time and this script takes no options: it installs the release it shipped with, into ~/.local/bin. To install a different release, fetch that release's own copy of this script — the URL is the version selector, so a pinned script and a pinned binary cannot drift apart.

set -eu

VERSION="@VERSION@"
REPO="https://github.com/valmirjunior0088/curios"
BIN_DIR="$HOME/.local/bin"

die() {
    echo "install: $*" >&2
    exit 1
}

case "$VERSION" in
    @*) die "this is the template, not a released installer; fetch it from $REPO/releases/latest/download/install.sh" ;;
esac

# The three platforms the release workflow builds. Anything else is refused by name rather than guessed at: a binary for the wrong architecture fails much less clearly than this line does.
os=$(uname -s)
arch=$(uname -m)

case "$os $arch" in
    "Linux x86_64" | "Linux amd64") asset="curios-linux-x86_64" ;;
    "Linux aarch64" | "Linux arm64") asset="curios-linux-aarch64" ;;
    "Darwin arm64" | "Darwin aarch64") asset="curios-macos-aarch64" ;;
    *) die "no prebuilt binary for $os $arch; build from source with: cargo build --release --package curios" ;;
esac

if command -v curl > /dev/null 2>&1; then
    fetch() { curl -fsSL "$1" -o "$2"; }
elif command -v wget > /dev/null 2>&1; then
    fetch() { wget -q "$1" -O "$2"; }
else
    die "neither curl nor wget is available"
fi

if command -v sha256sum > /dev/null 2>&1; then
    digest_of() { sha256sum "$1" | cut -d' ' -f1; }
elif command -v shasum > /dev/null 2>&1; then
    digest_of() { shasum -a 256 "$1" | cut -d' ' -f1; }
else
    die "neither sha256sum nor shasum is available"
fi

work=$(mktemp -d)
staged="$BIN_DIR/.curios.$$"
trap 'rm -rf "$work" "$staged"' EXIT INT TERM

base="$REPO/releases/download/release/$VERSION"

echo "Downloading Curios $VERSION ($asset)"
fetch "$base/$asset" "$work/curios" || die "could not download $base/$asset"
fetch "$base/checksums.txt" "$work/checksums.txt" || die "could not download $base/checksums.txt"

# Both come from the release named above rather than from `latest`, so a release published mid-install cannot pair one version's binary with another's checksum. This catches a truncated download or a swapped asset; it is not a defense against a compromised release, since the digest travels the same channel the binary does.
expected=$(grep " $asset\$" "$work/checksums.txt" | cut -d' ' -f1)
[ -n "$expected" ] || die "checksums.txt names no entry for $asset"

actual=$(digest_of "$work/curios")
[ "$expected" = "$actual" ] || die "checksum mismatch for $asset: expected $expected, got $actual"

chmod +x "$work/curios"

# Run it before installing it, never after. The usual failure here is a system whose glibc is older than the one the release was built against, and a binary that cannot start should not reach PATH first and fail on somebody's real invocation.
version=$("$work/curios" --version 2> /dev/null) ||
    die "the downloaded binary does not run on this system (usually an older glibc than the release was built against); build from source with: cargo build --release --package curios"

# Read before the rename overwrites it. An install that silently replaces a working toolchain is one the reader cannot audit afterwards, and "which one did I have?" is exactly the question a version bump makes urgent.
previous=""
if [ -x "$BIN_DIR/curios" ]; then
    previous=$("$BIN_DIR/curios" --version 2> /dev/null) || previous="an existing binary that does not run"
fi

# Staged inside the destination so the final step is a rename within one filesystem: an interrupted install leaves either the old binary or the new one, never half of either.
mkdir -p "$BIN_DIR"
cp "$work/curios" "$staged"
mv "$staged" "$BIN_DIR/curios"

if [ -n "$previous" ]; then
    echo "$version installed to $BIN_DIR/curios, replacing $previous"
else
    echo "$version installed to $BIN_DIR/curios"
fi

case ":$PATH:" in
    *":$BIN_DIR:"*) ;;
    *) echo "note: $BIN_DIR is not on your PATH — add it with: export PATH=\"\$HOME/.local/bin:\$PATH\"" ;;
esac
