EXTENSION :=
ifeq ($(OS),Windows_NT)
EXTENSION := .exe
endif

.PHONY: target/release/curios-runtime$(EXTENSION)

target/release/curios-runtime$(EXTENSION):
	cargo build --release --package curios-runtime

curios-compiler/runtime: target/release/curios-runtime$(EXTENSION)
	cp $< $@
