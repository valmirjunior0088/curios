.SILENT:

build/memory.o: memory.c
	clang memory.c \
		--output=build/memory.o \
		--target=wasm32 \
		--no-standard-libraries \
		--compile \
		--optimize=3

build/object.o: object.c
	clang object.c \
		--output=build/object.o \
		--target=wasm32 \
		--no-standard-libraries \
		--compile \
		--optimize=3

build/program.o: program.crs
	stack run curios-compiler program.crs build/program.o

build/program.wasm: build/memory.o build/object.o build/program.o
	wasm-ld build/memory.o build/object.o build/program.o \
		-o build/program.wasm \
		-O 3

.PHONY: clean interpret serve

clean:
	git clean -X --force build

interpret: program.crs
	stack run curios-interpreter program.crs start

serve: build/program.wasm
	echo "Started server on http://localhost:8080"
	stack exec curios-server 8080 build