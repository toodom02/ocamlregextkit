# A Regular Expression Toolkit

OCamlregextkit is a library providing simple interface to operations on regular expressions, NFAs, and DFAs.

Written in OCaml, using the Dune build system.

## Installation

Requires 
- OCaml v4.14.0 or greater
- Dune v3.4 or greater

See [OCaml docs](https://ocaml.org/docs/up-and-running) for installation instructions.

## Documentation

Find documentation for the toolkit [here](https://toodom02.github.io/ocamlregextkit/)

Note: all modules are packaged into a single module `Regextkit`

## Demo

For demonstration purposes, `demo` is a program built using the toolkit which accepts two regular expressions on the command line, and either verifies that they are equivalent or prints a word that matches one expression but not the other.

### Build the Library

```bash
make
```

### Build the demo

```bash
cd demo
make
```

### Run the demo

```bash
dune exec ./demo.exe "<regex>" "<regex>"
```

To test the demo, run

```bash
make test
```

This will run a set of test cases, which checks correctness of basic parsing, equivalence, and difference of two regular expressions.

## Testing

The `testing` directory contains code to profile and run timing tests for equivalence and minimisation of DFAs.

Run `make profile` or `make test` to compile and execute the respective function.

## Make Targets

 - `make` Builds the library with dune
 - `make clean` Removes dune's build directory
 - `make doc` Generates documentation for the toolkit