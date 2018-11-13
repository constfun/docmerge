build:
	dune build @all

op_set.mli: build
	cmitomli _build/default/.docmerge.eobjs/op_set.cmi > op_set.mli
