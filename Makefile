.PHONY: build scratch test clean reformat

build:
	jbuilder build

scratch: build
	jbuilder exec cafec -- language/scratch.cf

test: build
	jbuilder runtest

clean:
	jbuilder clean

format:
	ocamlformat --inplace \
	  test/*.ml \
	  source/*.ml source/*.mli \
	  source/*/*.ml source/*/*.mli
