# Midterm 2 Practice Problems (165 points)

These are *optional* practice problems for Midterm 2. The main topics for
Midterm 2 are:

- higher order functions (Using them to write non recursive functions,
  implementing them for other kinds of ADTs)

- Nano (and variations of it), interpreters (which could involve changes to /
  variations on Nano), closures

- Semantics, evaluation strategy

You can run the test suite with `stack test` or `make`, like the PAs.

Feel free to discuss these problems with other students and over Piazza!

## Problem 1: Folds for Tables (50 points)
Complete the 4 problems described in [FoldTables.hs](src/Language/Nano2/FoldTables.hs).

## Problem 2: One Step Reduction (70 points)
Complete the functions `isValue`, `subst`, and `reduce1` in
[Reduce.hs](src/Language/Nano2/Reduce.hs). The requirements are described in
the file.

All test cases rely on `reduce1` being completed - there are no test cases
that test only `isValue` or `subst`.

## Problem 3: Removing the Let Bindings (45 poins)
Complete the functions `noLets` and `desugar` in
[Desugar.hs](src/Language/Nano2/Desugar.hs). The requirements are described
in the file.

The last 2 test cases for this problem require that `reduce1` from Problem 2
is completed.
