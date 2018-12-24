.PHONY: build build-native test

build: autoformat
	dune build index.bc.js
	cp _build/default/index.bc.js automerge/backend.js

autoformat:
	dune build @fmt index.bc.js --auto-promote

build-native:
	dune build @all

test: automerge/node_modules build
	cd automerge && time ./node_modules/mocha/bin/mocha

test-orig: automerge/node_modules
	cd automerge && time ./node_modules/mocha/bin/mocha test-orig

automerge/node_modules:
	cd automerge && yarn install

_build/default/.op_set.eobjs/%.cmi: build-native
	true

op_set.mli: _build/default/.op_set.eobjs/op_set.cmi
	cmitomli $^ > $@

clean:
	dune clean
