.PHONY: build scratch test clean reformat

build:
	jbuilder build

scratchi: build
	jbuilder exec cafec -- \
	  --interpret --print-parse-ast language/scratch.cf

scratchc: build
	jbuilder exec cafec -- --print-parse-ast language/scratch.cf

test: build
	jbuilder runtest

clean:
	jbuilder clean

format:
	ocamlformat --inplace \
	  test/*.ml \
	  src/*.ml src/*.mli \
	  src/*/*.ml src/*/*.mli
