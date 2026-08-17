# Kitchen-sink benchmark image — every toolchain in one arm64 container.
#
# Build from the REPO ROOT (the build needs the curios sources):
#   docker build --platform linux/arm64 -f curios-benchmarks/Dockerfile -t curios-benchmarks .
# Run on a single pinned core so the numbers are stable:
#   docker run --rm --cpuset-cpus 0 curios-benchmarks
#   docker run --rm --cpuset-cpus 0 -e D_TREES=23 -e N_LCG=100000000 -e K_CHAIN=1600 -e N_CHURN=75000000 -e N_SPINES=75000 curios-benchmarks
#
# Toolchains are installed the perf-correct way (flambda OCaml, fresh V8). Grain ships no arm64 binary and its source build needs esy (also arm64-less), so its wasm is cross-compiled in the amd64 stage below. The first build is heavy but fully layer-cached. Spots flagged "VERIFY:" may need a version nudge.

# --- Grain: compile .gr -> wasm in an amd64 stage ---------------------------
# The official x64 binary runs emulated at build time (on Apple Silicon); the wasm it emits is arch-neutral and runs on the final image's native arm64 wasmtime. This is the only way to get Grain output on arm64 (no arm64 binary, and the source build needs esy, which has no arm64 release).
FROM --platform=linux/amd64 debian:bookworm-slim AS grain
RUN apt-get update && apt-get install -y --no-install-recommends curl ca-certificates \
    && rm -rf /var/lib/apt/lists/*
RUN curl -sSL -o /usr/local/bin/grain \
      https://github.com/grain-lang/grain/releases/download/grain-v0.7.2/grain-linux-x64 \
    && chmod +x /usr/local/bin/grain
COPY curios-benchmarks/programs/lcg/lcg.gr     /w/lcg.gr
COPY curios-benchmarks/programs/trees/trees.gr /w/trees.gr
COPY curios-benchmarks/programs/chain/chain.gr /w/chain.gr
COPY curios-benchmarks/programs/churn/churn.gr /w/churn.gr
COPY curios-benchmarks/programs/spines/spines.gr /w/spines.gr
RUN cd /w \
    && grain compile lcg.gr   -o lcg_grain.wasm \
    && grain compile trees.gr -o trees_grain.wasm \
    && grain compile chain.gr -o chain_grain.wasm \
    && grain compile churn.gr -o churn_grain.wasm \
    && grain compile spines.gr -o spines_grain.wasm

# --- main image (arm64) -----------------------------------------------------
FROM rust:1-bookworm
ENV DEBIAN_FRONTEND=noninteractive

# --- system deps + opam (OCaml's package manager) ---------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
      cmake git curl ca-certificates xz-utils bash m4 opam \
    && rm -rf /var/lib/apt/lists/*

# Rust wasm target (Rust appears in BOTH the native and the wasm section)
RUN rustup target add wasm32-wasip1

# --- Node.js via NodeSource (fresh V8; also drives AssemblyScript + Grain) ---
# Debian's apt node is 18.x; NodeSource gives current LTS. Don't also apt node.
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - \
    && apt-get install -y --no-install-recommends nodejs \
    && rm -rf /var/lib/apt/lists/*

# --- OCaml via opam, with a flambda switch (the perf-correct compiler) -------
# apt's ocaml is 4.14 without flambda; flambda's inlining is worth ~10-30% on this kind of code and is what the Benchmarks Game uses.
# VERIFY: bump the version if 5.2.0+options isn't available in the opam repo.
ENV OPAMROOTISOK=1 OPAMYES=1
RUN opam init --bare --disable-sandboxing \
    && opam switch create 5.2.0 ocaml-variants.5.2.0+options ocaml-option-flambda
ENV PATH="/root/.opam/5.2.0/bin:${PATH}"

# --- wasmtime: the shared engine for the wasm section -----------------------
RUN curl -sSf https://wasmtime.dev/install.sh | bash
ENV PATH="/root/.wasmtime/bin:${PATH}"

# --- hyperfine: the timing harness ------------------------------------------
RUN curl -sSL -o /tmp/hf.tar.gz \
      https://github.com/sharkdp/hyperfine/releases/download/v1.20.0/hyperfine-v1.20.0-aarch64-unknown-linux-gnu.tar.gz \
    && tar -xzf /tmp/hf.tar.gz -C /tmp \
    && cp /tmp/hyperfine-*/hyperfine /usr/local/bin/ \
    && rm -rf /tmp/hf.tar.gz /tmp/hyperfine-*

# --- Lean 4 via elan (official installer; arm64 supported) ------------------
RUN curl -sSf https://elan.lean-lang.org/elan-init.sh | sh -s -- -y
ENV PATH="/root/.elan/bin:${PATH}"

# --- AssemblyScript + WASI shim ---------------------------------------------
# VERIFY: the shim's runtime surface (process.argv, console.log) against this asc.
RUN npm install -g assemblyscript \
    && mkdir -p /opt/as && cd /opt/as && npm init -y \
    && npm install @assemblyscript/wasi-shim

# --- Grain — handled by the amd64 `grain` stage above; its wasm is copied in near the end (no arm64 Grain toolchain exists).

# --- Curios (built from the repo sources) -----------------------------------
# Dockerfile.dockerignore is what keeps target/ and .artifacts/ out of the build context; these COPYs only decide layer granularity. Launcher first (make), then the compiler (vendored Binaryen via CMake — slow, layer-cached afterward).
WORKDIR /curios
COPY Cargo.toml Cargo.lock Makefile ./
COPY curios-abi curios-abi
COPY curios-archive curios-archive
COPY curios-archive-derive curios-archive-derive
COPY curios-utilities curios-utilities
COPY curios-num curios-num
COPY curios-parse curios-parse
COPY curios-print curios-print
COPY curios-wasm curios-wasm
COPY curios-cont curios-cont
COPY curios-ersd curios-ersd
COPY curios-core curios-core
COPY curios-analysis curios-analysis
COPY curios-cert curios-cert
COPY curios-elab curios-elab
COPY curios-prelude curios-prelude
COPY curios-prelude-archive curios-prelude-archive
COPY curios-text curios-text
COPY curios-unit curios-unit
COPY curios-pipeline curios-pipeline
COPY curios-package curios-package
COPY curios-profile curios-profile
COPY curios-js curios-js
COPY curios curios
COPY curios-binaryen curios-binaryen
COPY curios-runtime curios-runtime
RUN make curios/runtime \
    && cargo build --release --package curios
ENV PATH="/curios/target/release:${PATH}"

# --- the benchmark harness --------------------------------------------------
COPY curios-benchmarks curios-benchmarks
WORKDIR /curios/curios-benchmarks
# Grain wasm, precompiled in the amd64 stage (no arm64 Grain toolchain exists).
COPY --from=grain /w/lcg_grain.wasm   prebuilt/lcg_grain.wasm
COPY --from=grain /w/trees_grain.wasm prebuilt/trees_grain.wasm
COPY --from=grain /w/chain_grain.wasm prebuilt/chain_grain.wasm
COPY --from=grain /w/churn_grain.wasm prebuilt/churn_grain.wasm
COPY --from=grain /w/spines_grain.wasm prebuilt/spines_grain.wasm
ENTRYPOINT ["bash", "entrypoint.sh"]
