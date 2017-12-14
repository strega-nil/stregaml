build:
	jbuilder build

scratch: build
	python ./run_scratch.py

test: build
	python ./test.py

clean:
	jbuilder clean

reformat:
	refmt --in-place */*.re */*/*.re */*.rei */*/*.rei
