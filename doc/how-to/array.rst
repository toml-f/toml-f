Working with arrays
===================

TOML supports *array* data types: flat, ordered containers holding multiple values.
Arrays are dynamically sized (*i.e.* do not need specify the number of elements before using an array) and can contain elements of any type supported by TOML.

The following program parses and stores an array of integers then prints the values to stdout:

.. literalinclude:: array/array/app/parse_array.f90
  :caption: app/parse_array.f90
  :language: Fortran


The simplest way to parse an array-valued key in TOML Fortran is to use the ``get_value`` interface, which has an overload to handle arrays.
First, we need to use a temporary variable ``arr`` (of type ``toml_array``), as the array's elements can be of potentially any type.
``arr`` is automatically associated and allocated by ``get_value``, which we can use to determine how much space is needed to store the array's contents.
Finally, we must iterate over all elements of the temporary ``toml_array`` and assign them to an element in our final data structure ``arr_data`` (which is a standard Fortran array).

As a test, let's run the above program using the following TOML table (``array.toml``):

.. code-block:: toml
   :caption: array.toml

   data=[1,2,3,4]

This produces the following output:

.. code-block:: text

   data =            1           2           3           4

As mentioned above, TOML arrays can contain elements of any type supported by TOML.
The TOML standard even supports heterogeneous arrays containing elements of multiple different types.
TOML Fortran supports this through the generic ``toml_array`` type.

Elements of ``toml_array`` may be of any value but require extra processing before they can be used in most Fortran programs, since Fortran arrays must only contain elements of a single type known at compile time.
This is achieved through the generic ``get_value`` interface, which automatically coerces elements of the ``toml_array`` to the correct type based on the variable it is copied to.

.. tip::

   The value in ``get_value`` is an ``intent(out)`` argument, in case the input and output parameter are incompatible, it is not initialized and the actual value is dependent on the compiler settings.
   By passing an integer value to the optional *stat* argument, the procedure will return a non-zero value to indicate an error.


Accessing whole arrays
----------------------

If the entries of an array are all of the same type or can be safely casted to a common type it is possible to retrieve a whole array using a single call to ``get_value``.

.. literalinclude:: array/whole/app/main.f90
  :caption: app/parse_array.f90
  :language: Fortran

The build interface will allocate the array to the correct size and then iterate over all elements and assign them to the array.


Accessing nested arrays
-----------------------

TOML arrays can be nested to produce an *array-of-arrays*. Both the top-level and nested child arrays are potentially variable-length and can contain any TOML data-type (including further nested arrays).

The following program reads a TOML file with an array-of-arrays nested a single level deep, and prints its values to stdout:

.. literalinclude:: array/nested/app/nested_array.f90
  :caption: app/nested_array.f90
  :language: Fortran

First, we need to call ``get_value`` to get a pointer to the top-level array then iterate through its elements, calling ``get_value`` on each to get a pointer to the child array.
Finally, we iterate through the individual sub-arrays and process their elements one-by-one.

This approach has the advantage of being able to deal with nested arrays of arbitrary length, such as the TOML table below:

.. code-block:: toml

   data=[[1,2,3], [4,5], [6]]

Which produces the following output:

.. code-block:: text

   data(1,1) = 1
   data(1,2) = 2
   data(1,3) = 3
   data(2,1) = 4
   data(2,2) = 5
   data(3,1) = 6

.. tip::

   A TOML array-of-arrays does not necessarily map cleanly to a regular Fortran multidimensional array.
   Multidimensional Fortran arrays must be rectangular (*i.e.* rows must contain the same number of elements), so an array-of-arrays with variable sized nested arrays cannot be directly mapped to a primitive Fortran type and must be represented using a compound data type.
