#!/usr/bin/env bash
# Build the benchmark image. The Docker build needs the curios sources, which
# live *above* benchmarks/, so the build context is the repo root. Both the context
# and the Dockerfile path are derived from this script's own location, so it
# works from any directory. Extra args pass through (e.g. --no-cache).
set -euo pipefail
dir="$(dirname "${BASH_SOURCE[0]}")"   # benchmarks/, as invoked
docker build --platform linux/arm64 -f "$dir/Dockerfile" -t curios-bench "$@" "$dir/.."
