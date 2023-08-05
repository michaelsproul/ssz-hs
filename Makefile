.PHONY: test

test:
	cabal run -O2 ssz-hs-test -- --quickcheck-tests 1000
