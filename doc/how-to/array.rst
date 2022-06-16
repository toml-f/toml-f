Working with arrays
===================

TOML supports *array* data types: flat, ordered containers holding
multiple values. Arrays are dynamically sized (i.e. don’t need specify
the number of elements before using an array) and can contain elements
of any type supported by TOML.

The following program parses and stores an array of integers then prints
the values to stdout:

.. literalinclude:: table/array/src/parse_array.f90
  :caption: src/parse_array.f90
  :language: Fortran


The simplest way to parse an array-valued key in ``toml-f`` is to use
the ``get_value()`` function, which has an overload to handle arrays.
First, we need to use a temporary variable ``arr`` (of type
``toml_array``), as the array’s elements can be of potentially any type.
``arr`` is automatically associated and allocated by ``get_value``,
which we can use to determine how much space is needed to store the
array’s contents. Finally, we must iterate over all elements of the
temporary ``toml_array`` and assign them to an element in our final data
structure ``arr_data`` (which is a standard Fortran array).

As a test, let’s run the above program using the following TOML table
(``array.toml``):

.. code:: toml

   data=[1,2,3,4]

This produces the following output:

::

   data =            1           2           3           4

As mentioned above, TOML arrays can contain elements of any type
supported by TOML. The TOML standard even supports heterogeneous arrays
containing elements of multiple different types. ``toml-f`` supports
this through the generic ``toml_array`` type.

Elements of ``toml_array`` may be of any value but require extra
processing before they can be used in most Fortran programs, since
Fortran arrays must only contain elements of a single type known at
compile time. This is achieved through the generic ``get_value``
interface, which automatically coerces elements of the ``toml_array`` to
the correct type based on the variable it is copied to.

.. tip::
   ``get_value`` returns a default value of zero if the types of the input and
   output parameters are incompatible. It also sets the value of the
   ``get_value``\'s ``stat`` parameter to a nonzero value to indicate an
   error ocurred.
