all: static dynamic

static:
	xcodebuild -target qmc-static -configuration Release

dynamic:
	xcodebuild -target qmc-dynamic -configuration Release

static-linux:
	mkdir -p build
	clang++ -c -std=c++11 -O3 -Iqmc -Ilib -fPIC qmc/qmc.cpp -o build/qmc.o
	ar -cvq libqmc-static.a build/qmc.o