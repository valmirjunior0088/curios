#!/usr/bin/env bash
# Build every contestant, cross-check that they all agree, then time them with hyperfine in four tables: {lcg, binary-trees} x {native, wasm-on-wasmtime}.
# Curios is the subject compared in both halves of each program.
#
# Knobs (env vars): N_LCG, D_TREES, RUNS, WARMUP.
set -uo pipefail
cd "$(dirname "$0")"
ROOT="$PWD"          # absolute benchmarks dir; asc is invoked from elsewhere (see below)
mkdir -p .artifacts

# Allowlist, not a blocklist: default stdout to stderr for the whole script, so every command's output — this script's own or any subprocess's, present or future — lands on stderr unless explicitly reopened. `table` below reopens fd 3 (the real stdout) around just the `hyperfine` call, the one thing meant to reach it.
exec 3>&1
exec 1>&2

# OCaml's flambda compiler lives in the opam switch.
eval "$(opam env 2>/dev/null)" || true

N_LCG="${N_LCG:-100000000}"   # LCG iterations (~0.45s of Curios compute)
D_TREES="${D_TREES:-21}"      # tree depth: ~4.2M nodes; sums taken mod 1000003
RUNS="${RUNS:-5}"
WARMUP="${WARMUP:-1}"

# The wasi-shim's asconfig sets `lib: ./assembly`, a glob that only resolves when asc runs with the shim's install dir as cwd — so we invoke it from there.
AS_DIR=/opt/as
AS_CONFIG=./node_modules/@assemblyscript/wasi-shim/asconfig.json

# path to a program's Lean exe (built by the Lake package in its own dir)
lean_bin() { echo "programs/$1/.lake/build/bin/$2"; }

# --- build one program in every language ------------------------------------
build() {
  local prog=$1 stem=$2
  local dir="programs/$prog"
  echo ">> building $prog"

  # Rust — native + wasm from one source
  rustc -O "$dir/$stem.rs" -o ".artifacts/${stem}_rust"
  rustc -O --target wasm32-wasip1 "$dir/$stem.rs" -o ".artifacts/${stem}_rust.wasm"

  # OCaml — native, flambda -O3
  ocamlopt -O3 "$dir/$stem.ml" -o ".artifacts/${stem}_ocaml"
  rm -f "$dir"/*.cmi "$dir"/*.cmx "$dir"/*.o

  # Lean — a Lake package living in this program's dir. VERIFY: elan default toolchain.
  ( cd "$dir" && lake build )

  # AssemblyScript — wasm via WASI shim. Run from $AS_DIR (so the shim's relative `lib` glob resolves); in/out are absolute since cwd changes. The shim patches the built-in process/console, so the .ts uses them as globals (no imports).
  ( cd "$AS_DIR" && asc "$ROOT/$dir/$stem.ts" --config "$AS_CONFIG" -O3 \
      -o "$ROOT/.artifacts/${stem}_asc.wasm" )

  # Grain — precompiled to wasm in the amd64 build stage (no arm64 Grain toolchain);
  # editing a .gr therefore needs an image rebuild, not just a rerun.
  cp "prebuilt/${stem}_grain.wasm" ".artifacts/${stem}_grain.wasm"

  # Curios — self-contained native executable (embeds wasmtime + the .cwasm)
  curios compile "$dir/$stem.crs" -o ".artifacts/${stem}_curios"
}

# --- correctness cross-check: every language must print the same number ------
check() {  # check <prog> <stem> <arg> <js-file>
  local prog=$1 stem=$2 arg=$3 js="$4"
  local lean; lean="$(lean_bin "$prog" "$stem")"
  echo ">> checking $prog (input $arg) — every output should be identical"
  printf '  %-12s %s\n' rust       "$(.artifacts/${stem}_rust $arg)"
  printf '  %-12s %s\n' ocaml      "$(.artifacts/${stem}_ocaml $arg)"
  printf '  %-12s %s\n' node       "$(node $js $arg)"
  printf '  %-12s %s\n' lean       "$($lean $arg)"
  printf '  %-12s %s\n' curios     "$(echo $arg | .artifacts/${stem}_curios)"
  printf '  %-12s %s\n' rust-wasm  "$(wasmtime run .artifacts/${stem}_rust.wasm $arg)"
  printf '  %-12s %s\n' grain-wasm "$(wasmtime run .artifacts/${stem}_grain.wasm $arg)"
  printf '  %-12s %s\n' asc-wasm   "$(wasmtime run .artifacts/${stem}_asc.wasm $arg)"
}

table() {  # table "<title>" <hyperfine args...>
  local title=$1; shift
  echo; echo "============================================================"
  echo "$title"; echo "============================================================"
  hyperfine --warmup "$WARMUP" --runs "$RUNS" "$@" 1>&3
}

build lcg   lcg
build trees trees

echo
check lcg   lcg   8  programs/lcg/lcg.js
check trees trees 10 programs/trees/trees.js

# --- LCG --------------------------------------------------------------------
table "LCG (N=$N_LCG) — native targets" --export-markdown .artifacts/lcg-native.md \
  ".artifacts/lcg_rust $N_LCG" \
  ".artifacts/lcg_ocaml $N_LCG" \
  "node programs/lcg/lcg.js $N_LCG" \
  "$(lean_bin lcg lcg) $N_LCG" \
  "echo $N_LCG | .artifacts/lcg_curios"

table "LCG (N=$N_LCG) — wasm on wasmtime" --export-markdown .artifacts/lcg-wasm.md \
  "echo $N_LCG | .artifacts/lcg_curios" \
  "wasmtime run .artifacts/lcg_rust.wasm $N_LCG" \
  "wasmtime run .artifacts/lcg_grain.wasm $N_LCG" \
  "wasmtime run .artifacts/lcg_asc.wasm $N_LCG"

# --- trees -------------------------------------------------------------------
table "trees (D=$D_TREES) — native targets" --export-markdown .artifacts/trees-native.md \
  ".artifacts/trees_rust $D_TREES" \
  ".artifacts/trees_ocaml $D_TREES" \
  "node programs/trees/trees.js $D_TREES" \
  "$(lean_bin trees trees) $D_TREES" \
  "echo $D_TREES | .artifacts/trees_curios"

table "trees (D=$D_TREES) — wasm on wasmtime" --export-markdown .artifacts/trees-wasm.md \
  "echo $D_TREES | .artifacts/trees_curios" \
  "wasmtime run .artifacts/trees_rust.wasm $D_TREES" \
  "wasmtime run .artifacts/trees_grain.wasm $D_TREES" \
  "wasmtime run .artifacts/trees_asc.wasm $D_TREES"

echo; echo "Markdown tables written to .artifacts/*.md"
