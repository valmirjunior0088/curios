EXTENSION :=
ifeq ($(OS),Windows_NT)
EXTENSION := .exe
endif

.PHONY: target/release/curios-rt$(EXTENSION)

target/release/curios-rt$(EXTENSION):
	cargo build --release --package curios-rt

curios/runtime: target/release/curios-rt$(EXTENSION)
	cp $< $@
