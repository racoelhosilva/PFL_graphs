all: depend build test

depend:
	cabal build --only-dependencies --enable-tests --enable-benchmarks

build: src/ test/
	cabal build --enable-tests --enable-benchmarks all

test: dist-newstyle/
	cabal test all