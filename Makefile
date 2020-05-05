.PHONY: default build run test clean

default: build

build:
	dune build main.exe

run:
	dune exec ./main.exe

test:
	dune runtest

clean:
	dune clean
