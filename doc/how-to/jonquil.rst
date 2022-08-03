.. _jonquil:

Compatibility with JSON
=======================

The Jonquil project provides a flavor of TOML Fortran which can handle JSON.
It is available from the `toml-f/jonquil <https://github.com/toml-f/jonquil`__ repository and can be included using the same steps as shown for TOML Fortran in :ref:`integration`.

.. tip::

   All types and procedures in TOML Fortran and Jonquil are fully compatible.
   Everything that is possible with TOML Fortran can be done with Jonquil, they can be mixed and matched without restriction.
   In short, Jonquil is TOML Fortran.


Using Jonquil
-------------

You can follow all recipes and tutorials in this documentation using Jonquil by just replacing the *tomlf* module with the *jonquil* module.
All derived types, procedure names and interfaces use the *json_* prefix instead of *toml_*, with the exception of the *toml_table* type which becomes a *json_object*.

The following example program shows how to load JSON data from a string and access the value using the build interface.

.. literalinclude:: jonquil/app/main.f90
   :language: Fortran
   :caption: app/demo.f90
   :lines: 1-24, 46

In contrast to TOML Fortran the loaded data structure is always returned as a polymorphic value, which can be dispatched by using a *select type* construct or by obtaining a pointer using *cast_to_object* / *cast_to_array* as appropriate.

Jonquil promises seamless compatibility with TOML Fortran, the data structure we just loaded can be manipulated with any procedure from the *tomlf* module and vice versa.

.. literalinclude:: jonquil/app/main.f90
   :language: Fortran
   :caption: app/demo.f90
   :lines: 26-44

Since the *json_object* is just a flavor of the *toml_table* type, it can be used in the same way as any other table.
