Working with tables
===================

The central data structure in TOML are tables, they contain a map from a key (string) to any supported data type in TOML.
This recipes describe common scenarios for retrieving data from tables using the TOML Fortran library.


Accessing nested tables
-----------------------

Using nested tables provides the possibility to better group configuration data.
Since the TOML format always requires the full qualified path in each table header, it is easy for the user to identify where the current settings belong to.
On the other hand deeply nested tables with long table paths or path components make it more difficult to use and a good balance of short and expressive table names and meaningful subtables is required.

An example for an electronic structure code implementing different Hamiltonians is give below.

.. code-block:: toml

   [hamiltonian.dftb]
   scc = {}
   skf.format = "mio-1-1/{}-{}.skf"

   [analysis]
   calculate-forces = true

The deepest nested subtable with entries in this example is the *hamiltonian.dftb.skf* path.

Such layout in the configuration file will usually be mirrored in the actual implementation, with every table corresponding to a derived type describing the input.
For the example above in total six derived types for the individual tables are defined as

.. literalinclude:: table/nested/src/input.f90
   :caption: src/input.f90
   :language: Fortran
   :lines: 10-54

.. note::

   The representation in Fortran derived types looks lengthy compared to the actual TOML input.
   Consider that the 40 lines of Fortran code contain 50% comments describing the data types briefly for (future) developers.
   Of course, the user documentation of the input format will be much more extensive, containing descriptions for every table and every entry, including input ranges and unit conventions.
   The final input file provided by the user can be brief and expressive.

Staring with the root of the table which is read in the *simulation_input* there are two ways to obtain access to a subtable, first we get the *hamiltonian* subtable, which we defined as mandatory, using the ``get_value`` interface.
In case it is present a reference will be returned in the *child* pointer.
If no table is available in the input TOML Fortran will insert it into the root table and return the reference to the newly created table.
The *child* pointer can still be unassigned in case invalid input is provided, which will result in raising an error in the implementation shown below.

The alternative is to explicitly mark the subtable as optional, like for the *analysis* table, if no table is available or the entry is invalid the *child* pointer will not be assigned.
To differentiate those cases we can request the status information, check whether the operation was successful, and cleanly handle the error case.

.. literalinclude:: table/nested/src/input.f90
   :caption: src/input.f90
   :language: Fortran
   :lines: 58-90

The same happens for reading the *hamiltonian_input* and *dftb_input* entry.

.. literalinclude:: table/nested/src/input.f90
   :caption: src/input.f90
   :language: Fortran
   :lines: 92-140

Finally, we can implement reading the terminal subtables into the *scc_input*, *skf_input*, and *analysis_input*, where we retrieve the actual values using the ``get_value`` interface.
Note that we can conveniently define default values using the ``get_value`` interface.
For proper error handling we can retrieve add the optional *stat* argument as well.

.. literalinclude:: table/nested/src/input.f90
   :caption: src/input.f90
   :language: Fortran
   :lines: 142-179

For the small incomplete input as shown here the fine grained substructure seems like overengineered, and could be fully defined in the reading routine for the document root as well.
However, for larger program inputs such structure can help to ensure that input readers are properly modular and reusable.

.. tip::

   The allocation status of a component of derived can used instead of a separate boolean flag to indicate whether a feature should be activated.
   This avoids requiring conditional code inside a reader routine for conditionally handling entries depending on a boolean flag, instead they can be collected in a subtable.

.. dropdown:: Full source code

   The full module implementing the *simulation_input* reading

   .. literalinclude:: table/nested/src/input.f90
      :caption: src/input.f90
      :language: fortran

   The auxilary module providing the error handler

   .. literalinclude:: table/nested/src/error.f90
      :caption: src/error.f90
      :language: fortran


Iterating over keys
-------------------

An expressive way to organize data is by providing a table where the keys of each entry describe the object that should be initialized.
For example in a package manager the keys represent the dependency, where each dependency is declared in a subtable.
Furthermore, a convenience feature might be the possibility to just provide a string, which is interpreted as a version subentry.

The final usage of this in a *requirements* table could look like the snippet shown below.

.. code-block:: toml

   [requirements]
   stdlib = "^0.2.1"
   toml = "^1.0.0"
   cli2 = "^3.1.0"
   lapack.version = "^3.10.1"
   lapack.variant = "mkl|openblas"
   minpack = {git="https://github.com/fortran-lang/minpack@v2.0.0"}

The first three entries provide a string value, while the forth entry provides a subtable implicitly by using dotted key-value pairs and the last entry uses an inline table.

Here we want to focus on the iteration and the default initialization, the internal structure of the *requirement_type* is secondary for this example.
We provide the minimal definition only holding the name of the dependency for demonstration purposes.

.. literalinclude:: table/keys/src/requirements.f90
   :caption: src/requirements.f90
   :language: fortran
   :lines: 10-15

For the actual implementation of reading all entries from the table we will use an one-dimensional array of *requirement_type* values.
Using the ``get_keys`` method of the table we can obtain a list of all keys for the current table, the method will always allocate the ``list`` variable and we can safely allocate the *requirement_type* using the number of keys.
To obtain the subtable, the ``get_value`` interface can be used, it will return a pointer to the subtable, either created implicitly by using a dotted key-value pair or by an inline table as shown in the snippet above.
Finally, we can call the actual constructor of the *requirement_type* using the subtable references with the ``child`` pointer.

.. literalinclude:: table/keys/src/requirements.f90
   :caption: src/requirements.f90
   :language: fortran
   :lines: 19-66

The other scenario we want to support is the presence of a string rather than a subtable.
In this case the ``get_value`` interface will fail, while it provides an optional status argument to check for successful operation, we can more conveniently and idomatically verify the success by checking the state of the ``child`` pointer.
If there is no subtable to reference, *i.e.* because it is a key-value pair with a string entry, the ``child`` pointer will not be associated, which can be easily checked.
For this case we will again use the ``get_value`` interface, but this time to retrieve the entry into a deferred length character.
Again we can idomatically check the status of the operation using the allocation state of the variable and create the appropriate error message if needed.
Eventually, we have to provide the constructor of the requirements with a table, for this purpose we create a dummy table and set the entry at the version key to the just retrieved string.
The newly created dummy table can be associated with the ``child`` pointer and passed to the actual constructor.

The actual constructor for our example is very minimalistic and only recovers the name of the dependency which is passed as a separate argument.

.. literalinclude:: table/keys/src/requirements.f90
   :caption: src/requirements.f90
   :language: fortran
   :lines: 68-81

.. note::

   While we provide an error handler in the example, we also ensure that the allocation status of the *requirement_type* values communicates the status of the operation as well.

.. dropdown:: Full source code

   The full module implementing the *requirement_type* reading

   .. literalinclude:: table/keys/src/requirements.f90
      :caption: src/requirements.f90
      :language: fortran

   The auxilary module providing the error handler

   .. literalinclude:: table/keys/src/error.f90
      :caption: src/error.f90
      :language: fortran
