.SILENT:

.PHONY: interpret

interpret:
	stack exec curios-interpreter program.crs

build/index.html: src/index.html
	cp src/index.html build/index.html

build/memory.o: src/memory.c
	clang src/memory.c -o build/memory.o --target=wasm32 --no-standard-libraries -I include -c -O3

build/object.o: src/object.c
	clang src/object.c -o build/object.o --target=wasm32 --no-standard-libraries -I include -c -O3

build/program.o: program.crs
	stack exec curios-compiler program.crs build/program.o

build/output.wasm: build/memory.o build/object.o build/program.o
	wasm-ld build/memory.o build/object.o build/program.o -o build/output.wasm --allow-undefined --entry main -O3

.PHONY: compile

compile: build/index.html build/output.wasm
	echo "Started server on http://localhost:8080"
	stack exec curios-server 8080 build
