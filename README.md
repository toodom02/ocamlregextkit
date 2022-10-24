# A Regular Expression Toolkit

Regex-Toolkit is a library providing simple interface to operations on regular expressions, NFAs, and DFAs.

Written in OCaml.

## Installation

Requires OCaml v4.14.0 or greater. 

See [OCaml docs](https://ocaml.org/docs/up-and-running) for installation instructions.

## Demo

For demonstration purposes, `compregex` is an app built using the toolkit which accepts two regular expressions on the command line, and either verifies that they are equivalent or prints a word that matches one expression but not the other.

### Build the Library

```bash
make
```

### Build the demo

```bash
cd demo
make
```

### Run the app

```bash
./compregex [-options] "<regex>" "<regex>"
```

To test the app, run

```bash
make test
```

### Options

 - `-v` Output stages and timings of the program
 - `-d` Output constructed ASTs, NFAs, and DFAs for debugging
 - `-O` Use optimised DFA construction
 - `-help` Display this list of options
 - `--help` Disply this list of options


## Make Targets

 - `make` Builds binaries
 - `make depend` Generates list of dependencies
 - `make clean` Removes any built binaries and generated files
 - `make docs` Generates documentation for toolkit