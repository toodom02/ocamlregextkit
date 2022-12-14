# A Regular Expression Toolkit

Regex-Toolkit is a library providing simple interface to operations on regular expressions, NFAs, and DFAs.

Written in OCaml.

## Installation

Requires OCaml v4.14.0 or greater. 

See [OCaml docs](https://ocaml.org/docs/up-and-running) for installation instructions.

## Documentation

Find documentation for the toolkit [here](https://toodom02.github.io/regex-toolkit/)

## Demo

For demonstration purposes, `demo` is a program built using the toolkit which accepts two regular expressions on the command line, and either verifies that they are equivalent or prints a word that matches one expression but not the other.

### Build the Library

```bash
make
```

### Build the demo

```bash
cd demo
make demo
```

### Run the demo

```bash
./demo "<regex>" "<regex>"
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

 - `make` Builds binaries
 - `make depend` Generates list of dependencies
 - `make clean` Removes any built binaries and generated files
 - `make docs` Generates documentation for toolkit