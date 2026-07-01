.PHONY: target/release/curios-rt

target/release/curios-rt:
	cargo build --release --package curios-rt

curios/runtime: target/release/curios-rt
	cp $< $@
