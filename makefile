.PHONY: build scratch test clean reformat

build:
	jbuilder build

scratch: build
	python ./run_scratch.py

test: build
	python ./test.py

clean:
	jbuilder clean

format:
	ocamlformat --inplace cafec/*.ml cafec/*/*.ml cafec/*/*.mli
