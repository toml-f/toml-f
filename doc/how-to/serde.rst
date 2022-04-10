Serializable base class
=======================

This recipe shows how to create a serializable class based on TOML Fortran.
Currently, TOML Fortran does not define such a base class itself, therefore we define a loader and dumper interface for turning a file or connected unit into a data structure.
The abstract base class will implement the processing of the file or unit to a TOML data structure and pass it to a deferred procedure which the implementing class uses to define its mapping from and back to the TOML data structure.
This way an easily round-tripable data structure can be created and used in a variety of contexts.

.. note::

   TOML Fortran might provide such abstract base class in the future natively.

The base class can be defined as

.. literalinclude:: serde/src/serde_class.f90
   :caption: src/serde_class.f90
   :language: fortran

We also define a convenience error handler which holds the error message and signals its error status by its allocation state.

.. literalinclude:: serde/src/serde_error.f90
   :caption: src/serde_error.f90
   :language: fortran

An example for a serializable class based on the above base class is given below.

.. literalinclude:: serde/src/demo.f90
   :caption: src/demo.f90
   :language: fortran

The defined data class can in an application easily be loaded from a file, while the actual implementation does not have to deal with getting the TOML data structure from the file but can assume that if the configuration file was valid TOML it will be provided with a data structure to read from.
