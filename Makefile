.PHONY: build build-native test

build:
	dune build op_set.bc.js
	cp _build/default/op_set.bc.js automerge/backend/op_set.js

build-native:
	dune build @all

test: automerge/node_modules build
	cd automerge && yarn test && cd -

automerge/node_modules:
	cd automerge && yarn install && cd -

_build/default/.op_set.eobjs/%.cmi: build-native
	true

op_set.mli: _build/default/.op_set.eobjs/op_set.cmi
	cmitomli $^ > $@

clean:
	dune clean