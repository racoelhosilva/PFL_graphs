.PHONY: test

all: test

depend:
	cabal build --only-dependencies --enable-tests --enable-benchmarks

build: depend
	cabal build --enable-tests --enable-benchmarks all

test:
	cabal test all