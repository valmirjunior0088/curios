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

.PHONY: curios/web
CURIOS_WEB_TARGET_TRIPLE := wasm32-unknown-unknown
CURIOS_WEB_BIN := $(CARGO_TARGET_DIR)/$(CURIOS_WEB_TARGET_TRIPLE)/release/curios_web.wasm
CURIOS_WEB_BUNDLE := $(CARGO_TARGET_DIR)/curios/$(CURIOS_WEB_TARGET_TRIPLE)

curios/web:
	cargo build \
		--release \
		--package curios-web \
		--target "$(CURIOS_WEB_TARGET_TRIPLE)" \
		--target-dir "$(CARGO_TARGET_DIR)"

	wasm-bindgen \
		--target web \
		--out-dir "$(CURIOS_WEB_BUNDLE)" \
		"$(CURIOS_WEB_BIN)"

.PHONY: curios/profile
CURIOS_PROFILE_SOURCE ?= programs/hello_curios.crs

curios/profile:
	cargo run \
		--release \
		--package curios \
		--features profile \
		--target-dir "$(CARGO_TARGET_DIR)" \
		-- profile "$(CURIOS_PROFILE_SOURCE)"
