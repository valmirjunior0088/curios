.DEFAULT_GOAL := runtime

.PHONY: runtime

runtime:
	cargo build --release -p curios-runtime
	cp target/release/curios-runtime curios-compiler/src/runtime
