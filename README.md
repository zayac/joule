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

<a name="ifm2016"></a>[1] Pavel Zaichenkov, Olga Tveretina, Alex Shafarenko:
A Constraint Satisfaction Method for Configuring Non-local Service Interfaces.
IFM 2016: 474-488

<a name="ifmcloud2016"></a>[2] Pavel Zaichenkov, Olga Tveretina, Alex
Shafarenko: Configuring Cloud-Service Interfaces Using Flow Inheritance.
iFMCloud@IFM 2016: 27-34

<a name="ppl208"></a>[3] Clemens Grelck, Sven-Bodo Scholz, Alexander V.
Shafarenko: A Gentle Introduction to S-Net: Typed Stream Processing and
Declarative Coordination of Asynchronous Components. Parallel Processing
Letters 18(2): 221-237 (2008)
