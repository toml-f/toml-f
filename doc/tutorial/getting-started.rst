Getting started
===============

.. sectionauthor:: Sebastian Ehlert <@awvwgk>
.. image:: https://img.shields.io/badge/difficulty-beginner-brightgreen
   :alt: Difficulty: Beginner

This tutorial provides a gentle introduction to the use of TOML Fortran.
It will deal with reading as well as creating TOML data structures using the high-level build interface and discuss how to obtain a data structure from a TOML document or turn a data structure to TOML document again.

For this project we will be working with fpm, however you can use any build tool you are familar with, checkout the :ref:`integration guide <integration>` to find a matching setup.
We start with creating a minimal package manifest to use TOML Fortran in our fpm project.

.. code-block:: toml
   :caption: fpm.toml

   name = "demo"

   [dependencies]
   toml-f.git = "https://github.com/toml-f/toml-f.git"

The public TOML Fortran API is defined in the ``tomlf`` module, we will only use this module for this entire course.
The main data structures we are going to interact with are ``toml_table`` and ``toml_array`` instances, which we can conveniently manipulate with the generic interface ``get_value``.

.. literalinclude:: getting-started/src/reader.f90
   :language: fortran
   :caption: src/reader.f90

Note that we declare the TOML data structure as mutable, *i.e.* ``intent(inout)`` rather than just ``intent(in)``, as the ``get_value`` interface can modify the data structure.
We start with a simple test program which is not actually reading any TOML document, but just passing an empty table to our reader.

.. literalinclude:: getting-started/app/defaults.f90
   :language: fortran
   :caption: app/defaults.f90

The ``get_value`` interface for processing the TOML data structure ensures that the data structure is complete throughout the whole process of reading it and will add the requested nodes if there are not present or will fill them in with default values.
Convince yourself that the empty table indeed changed while reading by passing a serializer to it.

.. code-block:: fortran

   block
     use tomlf, only : toml_serialize

     print '(a)', "# Final TOML data structure"
     print '(a)', toml_serialize(table)
   end block
   ! Expected output:
   ! # Final TOML data structure
   ! [spectrum]
   ! data = [ ]
   ! reverse = false

This behavior is very convenient because it allows us to define our default values while defining how we read the TOML data structure.

.. note::

   The ``get_value`` build interface is only one way of accessing the TOML data structure provided by TOML Fortran.
   It takes an opinionated approach towards reading and modifying the data structure, which is suitable the majority of applications.

Now we will actually read a TOML document and pass it to our reader.

.. code-block:: toml
   :caption: input.toml

   title = "Example"
   spectrum.data = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]

We adapt our command line driver to read the file ``input.toml`` and output the values as before

.. literalinclude:: getting-started/app/readin.f90
   :language: fortran
   :caption: app/readin.f90

Running the program with fpm shows that we were able to read the correct values from the document

.. code-block:: text

   ❯ fpm run readin
   Title: 'Example'
   Entries: 10
   Spectrum: 0.00000000 1.00000000 2.00000000 3.00000000 4.00000000 5.00000000 6.00000000 7.00000000 8.00000000 9.00000000

You can again use the serializer to write the final data structure, if you want to check whether the ``get_value`` interface has added default values.

.. important::

   In this tutorial you have learned out to read simple data structures from TOML documents.
   You can now

   - define the logic to read data from TOML structures
   - provide default values in your parser as you define the input structure
   - read an actual TOML document from a file
   - write TOML documents from your data structures
