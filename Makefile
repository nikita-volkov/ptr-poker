.PHONY: test bench

build: clean format compile test

format:
	for path in $$(ls -d *.cabal); do cabal-fmt -c $$path || cabal-fmt -i $$path; done
	ormolu -ci $$(find . -name "*.hs" -not -path "./*.stack-work/*" -not -path "./dist/*" -not -path "./dist-newstyle/*" -not -path "./.git/*")

compile:
	cabal build --builddir dist/compile --enable-tests --enable-benchmarks -j +RTS -A128m -n2m -N -RTS --ghc-options="-Werror -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wunused-packages -Wno-name-shadowing"

test:
	cabal test --builddir dist/test --test-show-details always -j +RTS -A128m -n2m -N -RTS --ghc-options="-Werror -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wunused-packages -Wno-name-shadowing"

test-lax:
	cabal test --builddir dist/test-lax --test-show-details always -j +RTS -A128m -n2m -N -RTS

bench:
	cabal bench --builddir dist/bench -j +RTS -A128m -n2m -N -RTS

docs:
	cabal haddock --enable-documentation --builddir dist/docs

clean:
	rm -rf dist
