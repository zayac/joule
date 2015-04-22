#MDL / CSP-KPN demo

##Image segmentation algorithm based on k-means clustering 

This is an image segmentation based on k-means clustering algorithm described
as a Kahn Process Network.  The example demonstrates interface specification
capabilities of the Message Definition Language (MDL).  It is used to test an
algorithm that solves a CSP with constraints formulated in the MDL.  It
provides all files to test a full stack of developed tools that in order
perform:

1. Transform the source files of components stored in `.cpp` files.  The
   transformed sources represent an original code extended with macro
   variables, so the components can be configured specifically for the context
   after the CSP is solved;
2. Derive the interfaces for every component from the source files;
3. Construct constraints using the derived interfaces and a net list, which
   declares the connections between the components;
4. Run the algorithm that solves the CSP and SAT to find a solution that
   satisfies the constraints from the previous step;
5. Configure the components by generating the code for macro variables;
6. For each component generate API functions that receive and send messages,
   and are used mostly by run-time system.

As a result, we get a set of modified source files that are tuned specifically
for the context and are ready for use with any coordination layer that can
handle communication between component.

The tools are executed (after they are built) using `run.sh` script from the
repository root directory using the command:

```
sh run.sh build tests/image-processing/image-processing.netlist
```

Below we briefly overview files used in the example:

* `image-processing.netlist` file specifies the connections between components
  including channel names.  In the example the names of components are `read`,
  `denoise`, `init` and `kMeans`;
* `*.cpp` files are the original implementations of the components;
* `cal.h` is a header file that must be included in every component.  It
  contains declarations specific to the format of the interfaces;
* `environment.terms` represents an interface of the environment.  The output
  interface declares the format of the messages provided to the input of the
  network and the input interface specifies the expected output of the network.
  All terms in the interface of the environment must be ground.
* `*.shell` files overwrite information in components about channel routing, as
  well as provide facilities for renaming term labels.  The shell facilitates
  separation of concerns between components, because the interfaces of
  incompatible components can be tuned in the shell in order to achieve precise
  match.

The tools generate the following files that we keep in 'build' directory:

* `*.transformed.cpp` are modified source files that are generated after step
  1 described above;
* `*.terms` store the interface of the components;
* `*.json` store auxiliary information that is required during code generation;
* `image-processing.constraints` is an input for the CSP-KPN algorithm that contains the
  constraints for the components;
* `image-processing.solution` is a solution of the CSP-KPN algorithm;
* `*_CAL_FI_variables.h` are files that contain generated macros definitions.
  They are generated based on the solution of the CSP-KPN and fully configure
  the components for the specific context.
