.PHONY: build scratch test clean reformat

build:
	jbuilder build

scratch: build
	jbuilder exec cafec -- language/scratch.cf

test: build
	python ./test.py

clean:
	jbuilder clean

format:
	ocamlformat --inplace cafec/*.ml cafec/*/*.ml cafec/*/*.mli
