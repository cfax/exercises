.PHONY: clean
clean:
	cabal clean

.PHONY: build
build:
	cabal build --enable-tests --write-ghc-environment-files=always --allow-newer=base

.PHONY: test-all
test-all:
	cabal test all --enable-tests --test-show-details=direct

.PHONY: test-lecture1
test-lecture1:
	cabal test doctest-lecture1 --enable-tests --test-show-details=direct --allow-newer=base
	cabal run exercises-test --enable-tests --allow-newer=base --ghc-options=-dynamic -- -m "Lecture 1"

.PHONY: test-lecture2
test-lecture2:
	cabal test doctest-lecture2 --enable-tests --test-show-details=direct --allow-newer=base
	cabal run exercises-test --enable-tests --allow-newer=base --ghc-options=-dynamic -- -m "Lecture 2"

.PHONY: test-lecture3
test-lecture3:
	cabal test doctest-lecture3 --enable-tests --test-show-details=direct --allow-newer=base
	cabal run exercises-test --enable-tests --allow-newer=base --ghc-options=-dynamic -- -m "Lecture 3"

.PHONY: test-lecture4
test-lecture4:
	cabal run exercises-test --enable-tests --allow-newer=base --ghc-options=-dynamic -- -m "Lecture 4"
