Date time compatibility
=======================

TOML values can represent date times and are also accessible via the build interfaces *get_value* and *set_value*.
However, the internal representation of a date time value might not be compatible with other libraries dealing with date times.
This recipes show how the build interface can be extended to also support common date time libraries.

`datetime-fortran <https://github.com/wavebitscientific/datetime-fortran>`_
---------------------------------------------------------------------------

The *datetime-fortran* library provides an opaque object representing a date time value and several features for manipulating it.
An fpm project using both TOML Fortran and *datetime-fortran* can be setup using the following dependencies section in the manifest.

.. code-block:: toml

   [dependencies]
   toml-f.git = "https://github.com/toml-f/toml-f.git"
   datetime.git = "https://github.com/wavebitscientific/datetime-fortran.git"

To define a compatibility between the two derived types representing a date time value we extend the *get_value* and *set_value* generic interfaces.

.. literalinclude:: datetime/datetime-fortran/src/compat.f90
   :language: fortran
   :caption: src/compat.f90


`M_time <https://github.com/urbanjost/M_time>`_
-----------------------------------------------

The *M_time* library provides a transparent object representing a date time value, with a rich functional and object oriented API for inspecting and manipulating it.
An fpm project using both TOML Fortran and *M_time* can be setup using the following dependencies section in the manifest.

.. code-block:: toml

   [dependencies]
   toml-f.git = "https://github.com/toml-f/toml-f.git"
   M_time.git = "https://github.com/urbanjost/M_time.git"

To define a compatibility between the two derived types representing a date time value we extend the *get_value* and *set_value* generic interfaces.

.. literalinclude:: datetime/M_time/src/compat.f90
   :language: fortran
   :caption: src/compat.f90
