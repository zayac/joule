Joule is a constraint solver for hierarchical data types with the support of abstraction and encapsulation.  The types are terms that are specified in Data Description Language (DDL).  Relations on terms form a set of constraints.  The purpose of Joule solver is to find a satisfiable model that consists of well-formed terms for every term variable present.

The description of terms and constraints structure is yet to come.

## Installation

The solver is implemented in OCaml language and depends on the following packages (all of them can be install via [OPAM](http://opam.ocaml.org/) package manager):

* findlib
* core
* sexplib
* comparelib
* ocamlgraph
* ctypes
* menhir

The solver also heavily uses [PicoSAT](http://fmv.jku.at/picosat/) SAT solver (API version 953), so make sure that a linker is able to access the library via `-lpicologic` flag.

Installation details can be found in
[INSTALL.txt](https://github.com/zayac/joule/blob/master/INSTALL.txt)
file.

### PicoSAT library installation details

#### OS X

The library may be installed from an unofficial Homebrew repository:
https://github.com/mht208/homebrew-formal

#### Ubuntu

PicoSAT for Ubuntu is accessible from the official repository:

```
apt-get install picosat
```