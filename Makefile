.PHONY: build build-native test

build:
	dune build index.bc.js
	cp _build/default/index.bc.js automerge/backend.js

build-native:
	dune build @all

test: automerge/node_modules build
	time ./automerge/node_modules/mocha/bin/mocha ./automerge/test/backend_test.js

test-js: automerge/node_modules
	time ./automerge/node_modules/mocha/bin/mocha ./automerge/test/backend_test_orig.js

automerge/node_modules:
	cd automerge && yarn install && cd -

_build/default/.op_set.eobjs/%.cmi: build-native
	true

op_set.mli: _build/default/.op_set.eobjs/op_set.cmi
	cmitomli $^ > $@

clean:
	dune clean
