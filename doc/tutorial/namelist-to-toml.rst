.. _namelist-to-toml:

Converting from Namelist to TOML
=================================

.. sectionauthor:: GitHub Copilot <@copilot>
.. image:: https://img.shields.io/badge/difficulty-beginner-brightgreen
   :alt: Difficulty: Beginner

This tutorial demonstrates how to convert Fortran namelist-based input files to TOML format using TOML Fortran.
Many existing Fortran codes use namelists for configuration, and this guide shows how to transition to TOML while maintaining familiar workflows.

.. note::

   This tutorial is inspired by the `CERFACS COOP blog post on Fortran namelists <https://cerfacs.fr/coop/fortran-namelist-workedex>`_, which regularly directs visitors to TOML Fortran.


Background: Namelists vs TOML
------------------------------

Fortran namelists have been a standard way to handle configuration in scientific codes since Fortran 77/90.
They provide a simple key-value format for reading and writing groups of variables:

.. code-block:: fortran

   &simulation
     iterations = 200,
     nx = 128,
     ny = 128,
     dt = 0.002,
     viscosity = 0.0005,
     mesh_file = '../MESH/mesh_large.h5'
   /

While namelists are convenient, TOML offers several advantages:

- **Better readability**: TOML uses a more modern, human-friendly syntax
- **More data types**: TOML supports dates, times, and nested structures more naturally
- **Validation**: Easier to validate and schema-check TOML files
- **Cross-language**: TOML parsers exist for many languages (Python, Rust, etc.)
- **No compilation needed**: You can work with TOML files without recompiling your code


Example: Simulation Configuration
----------------------------------

Let's convert a typical simulation configuration from namelist to TOML format.

Namelist Approach
~~~~~~~~~~~~~~~~~

First, let's look at how you would traditionally handle configuration with namelists:

.. literalinclude:: namelist-to-toml/simulation.nml
   :language: fortran
   :caption: simulation.nml

The Fortran code to read this would be:

.. literalinclude:: namelist-to-toml/src/namelist_reader.f90
   :language: fortran
   :caption: src/namelist_reader.f90

TOML Approach
~~~~~~~~~~~~~

The equivalent TOML configuration is more structured and readable:

.. literalinclude:: namelist-to-toml/simulation.toml
   :language: toml
   :caption: simulation.toml

The TOML Fortran reader is similarly straightforward:

.. literalinclude:: namelist-to-toml/src/toml_reader.f90
   :language: fortran
   :caption: src/toml_reader.f90


Migration Strategy
------------------

Converting from namelists to TOML can be done incrementally:

1. **Keep both formats**: Support both namelist and TOML files during transition
2. **Detect format**: Check file extension (``.nml`` vs ``.toml``) or try parsing
3. **Convert old files**: Write a simple converter script
4. **Validate inputs**: Use TOML's structure to validate configuration more thoroughly

Comparison Programs
~~~~~~~~~~~~~~~~~~~

Here's a program that reads the namelist format:

.. literalinclude:: namelist-to-toml/app/use_namelist.f90
   :language: fortran
   :caption: app/use_namelist.f90

And here's the equivalent using TOML:

.. literalinclude:: namelist-to-toml/app/use_toml.f90
   :language: fortran
   :caption: app/use_toml.f90


Advanced Example: Nested Configuration
---------------------------------------

TOML excels at representing nested and grouped data, which can be cumbersome in namelists.

Complex Namelist
~~~~~~~~~~~~~~~~

In namelists, you might have multiple groups:

.. code-block:: fortran

   &grid
     nx = 128
     ny = 128
     nz = 64
   /
   
   &solver
     max_iter = 1000
     tolerance = 1.0e-6
     method = 'GMRES'
   /
   
   &output
     frequency = 10
     format = 'HDF5'
     fields = 'velocity', 'pressure', 'temperature'
   /

Complex TOML
~~~~~~~~~~~~

TOML represents this more naturally with sections and subsections:

.. literalinclude:: namelist-to-toml/complex.toml
   :language: toml
   :caption: complex.toml

The TOML reader handles this elegantly:

.. literalinclude:: namelist-to-toml/src/complex_reader.f90
   :language: fortran
   :caption: src/complex_reader.f90


Building and Running
--------------------

You can try these examples using fpm. Create a simple package manifest:

.. literalinclude:: namelist-to-toml/fpm.toml
   :language: toml
   :caption: fpm.toml

Then build and run:

.. code-block:: bash

   fpm run use_namelist
   fpm run use_toml


Key Differences Summary
-----------------------

.. list-table:: Namelist vs TOML Comparison
   :header-rows: 1
   :widths: 30 35 35

   * - Feature
     - Namelist
     - TOML
   * - Syntax
     - ``&group ... /``
     - ``[section]``
   * - Comments
     - ``!`` (outside groups)
     - ``#`` (anywhere)
   * - Arrays
     - ``arr = 1, 2, 3``
     - ``arr = [1, 2, 3]``
   * - Strings
     - ``'text'`` or ``"text"``
     - ``"text"`` or ``'text'``
   * - Dates/Times
     - String representation
     - Native datetime type
   * - Nested structures
     - Multiple namelists
     - Natural table nesting
   * - Validation
     - Runtime only
     - Schema validation possible
   * - Cross-language
     - Fortran-specific
     - Many parsers available


Benefits of Migration
---------------------

Migrating from namelists to TOML provides several advantages:

1. **Modern syntax**: TOML is designed for modern configuration needs
2. **Better tooling**: Text editors have better TOML syntax highlighting
3. **Validation**: Easier to validate configuration before running simulations
4. **Documentation**: TOML structure makes configuration self-documenting
5. **Flexibility**: Easier to add new configuration options
6. **Interoperability**: Python scripts can easily read/modify TOML configs

.. important::

   In this tutorial, you have learned how to migrate from Fortran namelists to TOML.
   You can now:

   - Understand the differences between namelist and TOML syntax
   - Convert namelist configuration files to TOML format
   - Read TOML configuration in Fortran programs
   - Handle both simple and complex nested configurations
   - Maintain backward compatibility during migration


See Also
--------

- :ref:`getting-started` - Introduction to TOML Fortran basics
- :ref:`integration` - Integrating TOML Fortran in your project
- `CERFACS COOP Blog on Namelists <https://cerfacs.fr/coop/fortran-namelist-workedex>`_
- `TOML Specification <https://toml.io>`_
