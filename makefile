build:
	jbuilder build

scratch: build
	python ./run_scratch.py

test: build
	python ./test.py

clean:
	jbuilder clean
