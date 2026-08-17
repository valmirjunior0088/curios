#!/usr/bin/env bash
# Build every contestant, cross-check that they all agree, then time them with hyperfine in eight tables: {lcg, binary-trees, chain, churn} x {native, wasm-on-wasmtime}.
# Curios is the subject compared in both halves of each program.
#
# Knobs (env vars): N_LCG, D_TREES, K_CHAIN, N_CHURN, RUNS, WARMUP.
set -uo pipefail
cd "$(dirname "$0")"
ROOT="$PWD"          # absolute benchmarks dir; asc is invoked from elsewhere (see below)
mkdir -p .artifacts

# Allowlist, not a blocklist: default stdout to stderr for the whole script, so every command's output — this script's own or any subprocess's, present or future — lands on stderr unless explicitly reopened. `table` below reopens fd 3 (the real stdout) for one thing only: the markdown tables.
# That split is what makes the run's product retrievable without a bind mount. The container is `--rm` and takes its filesystem with it, so a table exported to a file inside it is gone; a table on stdout is captured by `docker run … > run.md` from the host. Everything else — the build log, the cross-check, hyperfine's own comparison — stays on stderr, so a failed run says why there and the document stays empty rather than half-written.
exec 3>&1
exec 1>&2

# OCaml's flambda compiler lives in the opam switch.
eval "$(opam env 2>/dev/null)" || true

N_LCG="${N_LCG:-100000000}"   # LCG iterations (~0.45s of Curios compute)
D_TREES="${D_TREES:-21}"      # tree depth: ~4.2M nodes; sums taken mod 1000003
K_CHAIN="${K_CHAIN:-1600}"    # transform rounds over a fixed 10 000-cell chain: ~16M cells reborn
N_CHURN="${N_CHURN:-75000000}" # record-update steps over a six-field record (~0.33s of Curios compute)
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

  # Exported through a temp file rather than straight to /dev/fd/3: hyperfine opens its export path itself, and when fd 3 is a regular file rather than a pipe that second open starts writing at offset zero, so six tables would overwrite each other. A table that hyperfine failed to produce contributes nothing to the document; its error is already on stderr.
  local md; md="$(mktemp)"
  hyperfine --warmup "$WARMUP" --runs "$RUNS" --export-markdown "$md" "$@"
  if [[ -s $md ]]; then
    { printf '## %s\n\n' "$title"; cat "$md"; echo; } >&3
  fi
  rm -f "$md"
}

build lcg   lcg
build trees trees
build chain chain
build churn churn

echo
check lcg   lcg   8  programs/lcg/lcg.js
check trees trees 10 programs/trees/trees.js
check chain chain 8  programs/chain/chain.js
check churn churn 8  programs/churn/churn.js

# --- LCG --------------------------------------------------------------------
table "LCG (N=$N_LCG) — native targets" \
  ".artifacts/lcg_rust $N_LCG" \
  ".artifacts/lcg_ocaml $N_LCG" \
  "node programs/lcg/lcg.js $N_LCG" \
  "$(lean_bin lcg lcg) $N_LCG" \
  "echo $N_LCG | .artifacts/lcg_curios"

table "LCG (N=$N_LCG) — wasm on wasmtime" \
  "echo $N_LCG | .artifacts/lcg_curios" \
  "wasmtime run .artifacts/lcg_rust.wasm $N_LCG" \
  "wasmtime run .artifacts/lcg_grain.wasm $N_LCG" \
  "wasmtime run .artifacts/lcg_asc.wasm $N_LCG"

# --- trees -------------------------------------------------------------------
table "trees (D=$D_TREES) — native targets" \
  ".artifacts/trees_rust $D_TREES" \
  ".artifacts/trees_ocaml $D_TREES" \
  "node programs/trees/trees.js $D_TREES" \
  "$(lean_bin trees trees) $D_TREES" \
  "echo $D_TREES | .artifacts/trees_curios"

table "trees (D=$D_TREES) — wasm on wasmtime" \
  "echo $D_TREES | .artifacts/trees_curios" \
  "wasmtime run .artifacts/trees_rust.wasm $D_TREES" \
  "wasmtime run .artifacts/trees_grain.wasm $D_TREES" \
  "wasmtime run .artifacts/trees_asc.wasm $D_TREES"

# --- chain -------------------------------------------------------------------
table "chain (K=$K_CHAIN) — native targets" \
  ".artifacts/chain_rust $K_CHAIN" \
  ".artifacts/chain_ocaml $K_CHAIN" \
  "node programs/chain/chain.js $K_CHAIN" \
  "$(lean_bin chain chain) $K_CHAIN" \
  "echo $K_CHAIN | .artifacts/chain_curios"

table "chain (K=$K_CHAIN) — wasm on wasmtime" \
  "echo $K_CHAIN | .artifacts/chain_curios" \
  "wasmtime run .artifacts/chain_rust.wasm $K_CHAIN" \
  "wasmtime run .artifacts/chain_grain.wasm $K_CHAIN" \
  "wasmtime run .artifacts/chain_asc.wasm $K_CHAIN"

# --- churn -------------------------------------------------------------------
table "churn (N=$N_CHURN) — native targets" \
  ".artifacts/churn_rust $N_CHURN" \
  ".artifacts/churn_ocaml $N_CHURN" \
  "node programs/churn/churn.js $N_CHURN" \
  "$(lean_bin churn churn) $N_CHURN" \
  "echo $N_CHURN | .artifacts/churn_curios"

table "churn (N=$N_CHURN) — wasm on wasmtime" \
  "echo $N_CHURN | .artifacts/churn_curios" \
  "wasmtime run .artifacts/churn_rust.wasm $N_CHURN" \
  "wasmtime run .artifacts/churn_grain.wasm $N_CHURN" \
  "wasmtime run .artifacts/churn_asc.wasm $N_CHURN"
