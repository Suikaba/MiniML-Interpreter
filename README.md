# MiniML Interpreter

[![Build Status](https://travis-ci.org/Suikaba/MiniML-Interpreter.svg?branch=master)](https://travis-ci.org/Suikaba/MiniML-Interpreter)

## Feature

- Types
  - Integer, Boolean, Unit
  - Function
  - Tuple
  - List
  - Reference
  - Variant
  - let-polymorphism (value-restriction)

- Expression
  - `+`, `-`, `/`, `*`, `<` on integer types
  - `&&`, `||` on boolean types
  - `if ... then ... else` statement
  - Function ; `fun arguments -> body`
  - Sequential execution ; `e1; e2`
  - Recursive function ; `let rec arguments = ...`
  - Tuple ; `e1, e2, ...`
  - List ; `[e1; e2; ...]`, `e1 :: tl` , `l1 @ l2`
  - Variant ;
  - Pattern matching ; `let *pattern* = ...` , `match expr with ...`


## Bulid and Test
- `dune build` to build.
- `dune exec miniml` to invoke the interpreter.
- `dune exec miniml file-path` to invoke the interpreter and exec input file
- `dune runtest` to run tests

