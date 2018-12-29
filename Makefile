.PHONY: build build-native test

build: autoformat
	dune build index.bc.js
	cd _build/default && \
		tail -1 index.bc.js | cut -c 51- | base64 -D > index.source.map && \
		cat index.source.map | sed 's/sourceRoot..../sourceRoot":"\/Users\/nick\/projects\/docmerge\/"/' > index.source.map.fixed && \
		sed "$d" index.bc.js > index.without.source.map.js && \
		printf "//# sourceMappingURL=data:application/json;base64," > index.source.map.fixed.encoded && \
		cat index.source.map.fixed | base64 >> index.source.map.fixed.encoded && \
		cat index.without.source.map.js index.source.map.fixed.encoded > index.js
	cp _build/default/index.js automerge/backend.js

autoformat:
	dune build @fmt index.bc.js --auto-promote

build-native:
	dune build @all

test: automerge/node_modules build
	cd automerge && ./node_modules/mocha/bin/mocha

test-orig: automerge/node_modules
	cd automerge && ./node_modules/mocha/bin/mocha test-orig

automerge/node_modules:
	cd automerge && yarn install

_build/default/.op_set.eobjs/%.cmi: build-native
	true

op_set.mli: _build/default/.op_set.eobjs/op_set.cmi
	cmitomli $^ > $@

clean:
	dune clean
