all: qmccpp
	LIBRARY_PATH=$(LIB_DIRS) cabal build

MOD=Main

LIB_DIRS="`pwd`/qmc-cpp"

qmccpp:
	$(MAKE) -C qmc-cpp lib

tests : qmccpp
	cabal test

ghci: qmccpp
	cabal exec -- ghci $(MOD) -isrc -itest