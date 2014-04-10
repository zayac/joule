Joule is a constraint solver for hierarchical data types with the support of abstraction and encapsulation.  The types are terms that are specified in Data Description Language (DDL).  Relations on terms form a set of constraints.  The purpose of Joule solver is to find a satisfiable model that consists of well-formed terms for every term variable present.

The description of terms and constraints structure is yet to come.

## Installation

Joule is being implemented in OCaml language. Therefore, you need to have a fully-fledged OCaml compiler installed in your system. The solver depends on the following OCaml packages:

* findlib
* core
* sexplib
* comparelib
* ocamlgraph
* ctypes
* menhir

It is suggested to install them via [OPAM](http://opam.ocaml.org/) package manager:

```
opam install core sexplib comparelib ocamlgraph ctypes menhir oasis
```

The solver also uses [PicoSAT](http://fmv.jku.at/picosat/) SAT solver (API version 953), so the PicoSAT library must be available in the system. The linker accesses it via `-lpicologic` flag.

### PicoSAT library installation details

#### OS X

The recommended way is to use [Homebrew](http://brew.sh/) to install PicoSAT library. The link below points to the stable version of the library that is available from [an unofficial Homebrew repository](https://github.com/mht208/homebrew-formal).

```
brew install https://raw.githubusercontent.com/mht208/homebrew-formal/73f246e453c35f144074d82518c475e7324e2a2e/picosat.rb
```

#### Ubuntu

PicoSAT for Ubuntu is accessible from the official repository:

```
apt-get install picosat
```