default: test

all: qmccpp-static
	cabal build

MOD=Main


qmccpp-static:
	$(MAKE) -C qmc-cpp static

qmccpp-dynamic:
	$(MAKE) -C qmc-cpp dynamic

test: qmccpp-static
	cabal test

ghci: qmccpp-dynamic
	cabal exec -- ghci $(MOD) -isrc -itest -lqmc-dynamic -Lqmc-cpp