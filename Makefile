default: test

all: qmccpp-static
	cabal build

MOD=Main


qmccpp-static:
	$(MAKE) -C qmc-cpp static

qmccpp-dynamic:
	$(MAKE) -C qmc-cpp dynamic

test: qmccpp-static
	cabal test --with-gcc=/usr/bin/gcc

ghci:
	cabal exec -- ghci $(MOD) -isrc -itest -lqmc-dynamic -Lqmc-cpp

ghci-test:
	cabal exec -- ghci Spec -isrc -itest -lqmc-dynamic -Lqmc-cpp
