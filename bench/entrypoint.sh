#!/usr/bin/env bash
# Build every contestant, cross-check that they all agree, then time them with
# hyperfine in four tables: {lcg, binary-trees} x {native, wasm-on-wasmtime}.
# Curios is the subject compared in both halves of each program.
#
# Knobs (env vars): N_LCG, D_TREES, RUNS, WARMUP.
set -uo pipefail
cd "$(dirname "$0")"
ROOT="$PWD"          # absolute bench dir; asc is invoked from elsewhere (see below)
mkdir -p bin

# OCaml's flambda compiler lives in the opam switch.
eval "$(opam env 2>/dev/null)" || true

N_LCG="${N_LCG:-100000000}"   # LCG iterations (~0.45s of Curios compute)
D_TREES="${D_TREES:-21}"      # tree depth: ~4.2M nodes; sums taken mod 1000003
RUNS="${RUNS:-5}"
WARMUP="${WARMUP:-1}"

# The wasi-shim's asconfig sets `lib: ./assembly`, a glob that only resolves when
# asc runs with the shim's install dir as cwd — so we invoke it from there.
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
  rustc -O "$dir/$stem.rs" -o "bin/${stem}_rust"
  rustc -O --target wasm32-wasip1 "$dir/$stem.rs" -o "bin/${stem}_rust.wasm"

  # OCaml — native, flambda -O3
  ocamlopt -O3 "$dir/$stem.ml" -o "bin/${stem}_ocaml"
  rm -f "$dir"/*.cmi "$dir"/*.cmx "$dir"/*.o

  # Lean — a Lake package living in this program's dir. VERIFY: elan default toolchain.
  ( cd "$dir" && lake build )

  # AssemblyScript — wasm via WASI shim. Run from $AS_DIR (so the shim's relative
  # `lib` glob resolves); in/out are absolute since cwd changes. The shim patches
  # the built-in process/console, so the .ts uses them as globals (no imports).
  ( cd "$AS_DIR" && asc "$ROOT/$dir/$stem.ts" --config "$AS_CONFIG" -O3 \
      -o "$ROOT/bin/${stem}_asc.wasm" )

  # Grain — precompiled to wasm in the amd64 build stage (no arm64 Grain toolchain);
  # editing a .gr therefore needs an image rebuild, not just a rerun.
  cp "prebuilt/${stem}_grain.wasm" "bin/${stem}_grain.wasm"

  # Curios — self-contained native executable (embeds wasmtime + the .cwasm)
  curios-compiler compile "$dir/$stem.crs" -o "bin/${stem}_curios"
}

# --- correctness cross-check: every language must print the same number ------
check() {  # check <prog> <stem> <arg> <js-file>
  local prog=$1 stem=$2 arg=$3 js="$4"
  local lean; lean="$(lean_bin "$prog" "$stem")"
  echo ">> checking $prog (input $arg) — every output should be identical"
  printf '  %-12s %s\n' rust       "$(bin/${stem}_rust $arg)"
  printf '  %-12s %s\n' ocaml      "$(bin/${stem}_ocaml $arg)"
  printf '  %-12s %s\n' node       "$(node $js $arg)"
  printf '  %-12s %s\n' lean       "$($lean $arg)"
  printf '  %-12s %s\n' curios     "$(echo $arg | bin/${stem}_curios)"
  printf '  %-12s %s\n' rust-wasm  "$(wasmtime run bin/${stem}_rust.wasm $arg)"
  printf '  %-12s %s\n' grain-wasm "$(wasmtime run bin/${stem}_grain.wasm $arg)"
  printf '  %-12s %s\n' asc-wasm   "$(wasmtime run bin/${stem}_asc.wasm $arg)"
}

table() {  # table "<title>" <hyperfine args...>
  local title=$1; shift
  echo; echo "============================================================"
  echo "$title"; echo "============================================================"
  hyperfine --warmup "$WARMUP" --runs "$RUNS" "$@"
}

build lcg   lcg
build trees trees

echo
check lcg   lcg   8  programs/lcg/lcg.js
check trees trees 10 programs/trees/trees.js

# --- LCG --------------------------------------------------------------------
table "LCG (N=$N_LCG) — native targets" --export-markdown bin/lcg-native.md \
  "bin/lcg_rust $N_LCG" \
  "bin/lcg_ocaml $N_LCG" \
  "node programs/lcg/lcg.js $N_LCG" \
  "$(lean_bin lcg lcg) $N_LCG" \
  "echo $N_LCG | bin/lcg_curios"

table "LCG (N=$N_LCG) — wasm on wasmtime" --export-markdown bin/lcg-wasm.md \
  "echo $N_LCG | bin/lcg_curios" \
  "wasmtime run bin/lcg_rust.wasm $N_LCG" \
  "wasmtime run bin/lcg_grain.wasm $N_LCG" \
  "wasmtime run bin/lcg_asc.wasm $N_LCG"

# --- trees -------------------------------------------------------------------
table "trees (D=$D_TREES) — native targets" --export-markdown bin/trees-native.md \
  "bin/trees_rust $D_TREES" \
  "bin/trees_ocaml $D_TREES" \
  "node programs/trees/trees.js $D_TREES" \
  "$(lean_bin trees trees) $D_TREES" \
  "echo $D_TREES | bin/trees_curios"

table "trees (D=$D_TREES) — wasm on wasmtime" --export-markdown bin/trees-wasm.md \
  "echo $D_TREES | bin/trees_curios" \
  "wasmtime run bin/trees_rust.wasm $D_TREES" \
  "wasmtime run bin/trees_grain.wasm $D_TREES" \
  "wasmtime run bin/trees_asc.wasm $D_TREES"

echo; echo "Markdown tables written to bin/*.md"
