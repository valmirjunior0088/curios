.SILENT:

build/index.html: deps/index.html
	cp deps/index.html build/index.html

build/memory.o: deps/memory.c
	clang deps/memory.c -o build/memory.o --target=wasm32 --no-standard-libraries -I include -c -O3

build/object.o: deps/object.c
	clang deps/object.c -o build/object.o --target=wasm32 --no-standard-libraries -I include -c -O3

build/program.o: program.crs
	stack exec curios-compiler program.crs build/program.o

build/output.wasm: build/memory.o build/object.o build/program.o
	wasm-ld build/memory.o build/object.o build/program.o -o build/output.wasm --allow-undefined --entry main -O3

.DEFAULT_GOAL :=

default: build/index.html build/output.wasm
	echo "Started server on http://localhost:8080"
	stack exec curios-server 8080 build
