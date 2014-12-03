This is a set of tools that serves a mechanism for interface reconciliation in
[Kahn Process Networks](http://en.wikipedia.org/wiki/Kahn_process_networks) and
is intended to be used in a component programming language based on streaming
networks.  Meanwhile, the tools implement a Constraint Aggregation Layer in
[AstraKahn](http://arxiv.org/abs/1306.6029) programming language.  The core of
the mechanism is a constraint solver Joule.  It resolves a set of constraints
that defines relations between input and output message format in the streaming
network.

The project consists of the following parts:

### Component Transformer

Transformer modifies a single component source file defined in C++ (a component
protocol description is yet to come).

### Term Derivation Tool

A tool infers input and output message interfaces described in Message
Definition Language (MDL) (see AstraKahn preprint using the link above) from
component's code.  It generates a '.term' file that contains description of the
interface.

### Constraint Generator

Given description of the network (defined as a netlist in a '.netlist' file)
and a set of '.terms' files, the tool generates constraints for components that
are connected via a channel and stores them in a '.constraints' file.

### Joule

Joule is a constraint solver for satisfying relations between message formats
in the network described in MDL.  The purpose of Joule is to "contextualise"
components (similar to template specialization in C++).  For all term variables
the solver finds a satisfiable model, which represents a data format that can
be used in the context of the network.

### Code Generator

TBD

### Channel Splitter/Merger

TBD
