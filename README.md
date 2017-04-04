# Interface Configuration Mechanism for Web Services

[[1]](#ifm2016) and [[2]](#ifmcloud2016) present a formal method for automatic
configuration of loosely coupled web services in the presence of subtyping,
parametric polymorphism, Boolean variables, which are used to control
dependencies between any elements of interface collections, and flow
inheritance [[3]](#ppl2008).  It proposes the Message Definition Language (MDL)
as an Interface Description Language (IDL) for specifying interfaces of the web
services.  Similarly to [Protocol
Buffers](https://developers.google.com/protocol-buffers/), the MDL specifies
functionality of web services and data formats that the service is compatible
with.

However, none of the existing IDLs provides support for flexible and
configurable interfaces.  We addressed this challenged and developed an
interface configuration mechanism on top of the MDL.  The configuration
mechanism is implemented as the set of tools for web services written in C++,
which we provide in this repository.

## Description

Using the tools provided, the configuration mechanism automatically derives the
interfaces from the services, constructs a set of communication constraints,
solves the constraint satisfaction problem (CSP), configures the services using
the solution produced by the solver and, finally, generates a library for each
service in the application.  The library contains the implementation of
services specific to data formats in the given environment.  A solution to the
CSP guarantees that the data formats, which are sent and received from the
library, are compatible with data formats in other services.

### Service transformation

As the first step of the configuration protocol, each service source code is
augmented with macros acting as placeholders for the code that enables flow
inheritance. After the CSP is solved, the values for configuration parameters
are propagated back to the services via header files with definitions for the
macros.

### Interface derivation

Our mechanism supports automatic interface derivation from the source code of
the services provided that service implementation follow certain guidelines.
The interfaces are derived in a form of the MDL terms.

### Constraint generation

Given the derived interfaces and the topology of the application, at this step
the mechanism constructs a set of the communication constraints.

### Constraint satisfaction (Joule)

Joule is the constraint satisfaction solver for communication constraints that
finds values for configuration parameters in the interfaces.

The constraints ensure the compatibility of data formats.  The CSP is solved if
for all Boolean variables and term variables in the problem we find values that
satisfy the constraints.

### Code generation

Finally, after the CSP is solved, the values for the configuration parameters
are propagated back to the source code of the services.  For each service we
generate a header file that contains macro definitions for each value.  Given
a header file, each service is compiled to a binary, which exposes its API to
be used in a service-based application.

## Compiling the Solver

The main contribution of this work is implementation of the solver that
satisfies communication constraints for interfaces of a service-based
application.  The solver is implemented in OCaml and uses
[PicoSAT](http://fmv.jku.at/picosat/) for Boolean satisfiability.

To build the solver, the following steps need to be perform (the build process
is illustrated in Ubuntu).

```shell
# make sure that dependencies for the solver are installed
apt-get install ocaml opam m4 libffi-dev picosat
# initialize the OCaml package manager
opam init
eval `opam config env`
# install OCaml libraries that are required for compiling the solver
opam install ocamlfind menhir comparelib core ctypes ctypes-foreign ocamlgraph sexplib
# download sources and cd to the build directory
git clone https://github.com/zayac/joule
cd joule/joule
# configure the setup and build the solver
./configure
make
```

## References

<a name="ifm2016">[1]</a> Pavel Zaichenkov, Olga Tveretina, Alex Shafarenko:
A Constraint Satisfaction Method for Configuring Non-local Service Interfaces.
IFM 2016: 474-488

<a name="ifmcloud2016">[2]</a> Pavel Zaichenkov, Olga Tveretina, Alex
Shafarenko: Configuring Cloud-Service Interfaces Using Flow Inheritance.
iFMCloud@IFM 2016: 27-34

<a name="ppl2008">[3]</a> Clemens Grelck, Sven-Bodo Scholz, Alexander V.
Shafarenko: A Gentle Introduction to S-Net: Typed Stream Processing and
Declarative Coordination of Asynchronous Components. Parallel Processing
Letters 18(2): 221-237 (2008)
