CARGO_TARGET_DIR ?= target

.PHONY: curios/runtime
CURIOS_RUNTIME_TARGET_TRIPLE := $(shell rustc -vV | sed -n 's/^host: //p')
CURIOS_RUNTIME_BIN := $(CARGO_TARGET_DIR)/curios/$(CURIOS_RUNTIME_TARGET_TRIPLE)/runtime

curios/runtime:
	cargo build \
		--release \
		--package curios-runtime \
		--target "$(CURIOS_RUNTIME_TARGET_TRIPLE)" \
		--target-dir "$(CARGO_TARGET_DIR)"

	mkdir -p "$(dir $(CURIOS_RUNTIME_BIN))"

	cp \
		"$(CARGO_TARGET_DIR)/$(CURIOS_RUNTIME_TARGET_TRIPLE)/release/curios-runtime" \
		"$(CURIOS_RUNTIME_BIN)"

.PHONY: curios/js
CURIOS_JS_TARGET_TRIPLE := wasm32-unknown-unknown
CURIOS_JS_BIN := $(CARGO_TARGET_DIR)/$(CURIOS_JS_TARGET_TRIPLE)/release/curios_js.wasm
CURIOS_JS_BUNDLE := $(CARGO_TARGET_DIR)/curios/$(CURIOS_JS_TARGET_TRIPLE)

curios/js:
	cargo build \
		--release \
		--package curios-js \
		--target "$(CURIOS_JS_TARGET_TRIPLE)" \
		--target-dir "$(CARGO_TARGET_DIR)"

	wasm-bindgen \
		--target web \
		--out-dir "$(CURIOS_JS_BUNDLE)" \
		"$(CURIOS_JS_BIN)"

.PHONY: curios/profile
CURIOS_PROFILE_SOURCE ?= programs/hello_world.crs

curios/profile:
	cargo run \
		--release \
		--package curios \
		--features profile \
		--target-dir "$(CARGO_TARGET_DIR)" \
		-- profile "$(CURIOS_PROFILE_SOURCE)"

.PHONY: benchmarks
BENCHMARKS_TAG ?= curios-benchmarks

benchmarks:
	docker build \
	    --platform linux/arm64 \
		--file benchmarks/Dockerfile \
		--tag "$(BENCHMARKS_TAG)" .

	docker run \
	    --rm \
		--cpuset-cpus 0 \
		"$(BENCHMARKS_TAG)"
