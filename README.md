# A Regular Expression Toolkit

Accepts two regular expressions on the command line, and either verifies that they are equivalent or prints a word that matches one expression but not the other.

Written in OCaml.

## Installation

Requires OCaml v4.14.0 or greater. 

See [OCaml docs](https://ocaml.org/docs/up-and-running) for installation instructions.

## Usage

### Build the app

```bash
make
```

### Run the app

```bash
./regextkit [-options] "<regex>" "<regex>"
```

### Options

 - `-v` Output stages and timings of the program
 - `-d` Output constructed ASTs, NFAs, and DFAs for debugging
 - `-O` Use optimised DFA construction
 - `-help` Display this list of options
 - `--help` Disply this list of options

### Make Targets

 - `make` Builds binaries
 - `make test` Executes suite of tests
 - `make depend` Generates list of dependencies
 - `make clean` Removes any built binaries and generated files