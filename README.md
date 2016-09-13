# bitblast
CNF encoding algorithms for arithmetical operations. Written in Haskell, with small parts in C++.

The (english) master thesis is [available for download](http://www.martin-finke.de/documents/Masterarbeit_bitblast_Finke.pdf).

## License

All rights reserved. For usage permission, please contact: martin (at) martin (dash) finke (dot) de.

## Installation

All dependencies can be installed by running:

```
cabal install --only-dependencies --enable-tests
```

The C++ library is located in the *qmc-cpp* directory and must be compiled to a library separately using the supplied *Makefile*. When running GHCi, the library must be linked dynamically. This can be done by running:

```
make ghci
```

## List of Modules
The Haskell modules are:

- `Arithmetics`: Formulas for arithmetical operations, based on both circuits and truth tables.
- `ArithmeticsModular`: Functions to chain two existing addition formulas to create an addition formula with a higher bit width.
- `Assignment`: Assignment data type and fundamental operations on it.
- `CalculatedFormulas`: The CNFs that were calculated.
- `EspressoInterface`: Interface to the ESPRESSO heuristic logic minimizer.
- `Formula`: Formula data type and fundamental operations on it.
- `LimpCBCInterface`: Interface to the *Coin-or branch and cut* (CBC) linear integer solver. Used to find a minimum subset of primes.
- `MinimizeFormula`: High-level functions to find equivalent and equisatisfiable CNFs for a given formula.
- `ModifiedQmc`: Modified Quine-McCluskey algorithm to find the set of non-redundant candidate clauses.
- `NormalForm`: Conversion of formulas to conjunctive normal form.
- `ParseCnf`: Parser to read a CNF from a string.
- `QmcCpp`: Interface to the Quine-McCluskey C++ library.
- `QmcTypes`: Fundamental types and operations related to the Quine-McCluskey algorithm.
- `SatchmoInterface`: Finds the minimum subset of primes using Satchmo and MiniSat.
- `SatchmoOutput`: Generates a Haskell module to be used with the con-test software, using minimized CNFs from the `CalculatedFormulas` module.
- `TruthBased`: Helper functions for the semantic method.
- `TruthBasedApprox`: Random approximate semantic method.
- `TruthBasedCore`: Semantic method, including removal of redundant clauses.
- `TruthBasedGenetic`: Genetic semantic method.
- `TruthBasedNaive`: Naive combinatorial implementation.
- `TruthTable`: Truth table data type and fundamental operations on it.
- `Tseitin`: Implementation of the Tseitin transform.
- `TseitinSelect`: Functions for finding the set of sub-formulas in a formula.
- `Variable`: Boolean variable data type.
- `WorthExtraVariables`: Iterate over all Boolean functions with a given number of variables to see if adding an extra variable reduces CNF size.

## Tests
A suite of 350 test cases can be run by executing:
```
cabal test
```
## Usage Examples
The following examples should serve as an overview of the functionality. They can be run in GHCi.

Creating arithmetical formulas with a bit width and overflow mode:

```haskell
nBitAddition Forbid 2
nBitMultiplication DontCare 3
greaterThan 3
greaterThanEq 2
```

Pretty-printing a formula:

```haskell
Formula.prettyPrint $ nBitAddition Forbid 4
```

Converting to conjunctive normal form:

```haskell
toCanonicalCnf $ nBitAddition Forbid 2
```

Finding a minimal equivalent CNF:

```haskell
minimizeFormula $ nBitMultiplication Forbid 2
```

Finding an equisatisfiable CNF with one extra variable using the syntactic method:

```haskell
minimizeStructural 1 $ greaterThan 3
```

Using the semantic method:

```haskell
minimizeTruthBased 1 $ nBitAddition Forbid 2 -- exact
minimizeGenetic 1 $ nBitAddition Forbid 2 -- genetic
```

Performing the Tseitin transform:

```haskell
fullTseitin $ nBitAddition Forbid 5
```

Getting the smallest calculated CNF for an operation:

```haskell
getBest Add 4 -- or Mul, GreaterThan, GreaterThanEq
```

Showing the size of a CNF:

```haskell
fmap getStats $ getBest Mul 5
```

Showing the set of sub-formulas in a formula:

```haskell
possibleReplacements $ greaterThan 2
```

Creating modules for the con-test software (the *con-test* folder must be in the parent folder of where GHCi is run):

```haskell
let bitWidths = [1..5]
outputNoExtra bitWidths -- smallest equivalent CNFs
outputBest bitWidths -- smallest equisatisfiable CNFs
```

This will create the files *OptNatNoExtra.hs* and *OptNatBest.hs* in the *con-test* folder.