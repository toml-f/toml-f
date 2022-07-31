Working with tables
===================

The central data structures in TOML are tables, they contain a map from a key (string) to any supported data type in TOML.
These recipes describe common scenarios for retrieving data from tables using the TOML Fortran library.


Accessing nested tables
-----------------------

Using nested tables provides the possibility to better group configuration data.
Since the TOML format always requires the full qualified path in each table header, it is easy for the user to identify where the current settings belong to.
On the other hand, deeply nested tables with long table paths or path components make them more difficult to use and a good balance of short and expressive table names and meaningful subtables is required.

An example of an electronic structure code implementing different Hamiltonians is given below.

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
For proper error handling, we can retrieve the optional *stat* argument as well.

.. literalinclude:: table/nested/src/input.f90
   :caption: src/input.f90
   :language: Fortran
   :lines: 142-179

For the small incomplete input as shown here, the fine-grained substructure seems overengineered and could be fully defined in the reading routine for the document root as well.
However, for larger program inputs such a structure can help to ensure that input readers are properly modular and reusable.

.. tip::

   The allocation status of a component of the derived type can be used instead of a separate boolean flag to indicate whether a feature should be activated.
   This avoids requiring conditional code inside a reader routine for conditionally handling entries depending on a boolean flag, instead they can be collected in a subtable.

.. dropdown:: Full source code

   The full module implementing the *simulation_input* reading

   .. literalinclude:: table/nested/src/input.f90
      :caption: src/input.f90
      :language: fortran

   The auxiliary module providing the error handler

   .. literalinclude:: table/nested/src/error.f90
      :caption: src/error.f90
      :language: fortran


Direct access via key paths
---------------------------

If only a deeply nested value of a data structure is needed it can be retrieved by using a key path.
The build interface will internally walk the key path, resolve the child tables and create them as necessary.

.. warning::

   Repeatly accessing values via a key path from the document root, rather than retrieving the reference the desired child table, will introduce an overhead each time the key path is resolved.

For the previous example we can use the key path access to retrieve the most deeply nested value as shown below.

.. code-block:: fortran

   block
     use tomlf, only : get_value, toml_path, toml_key
     character(:), allocatable :: format_string

     call get_value(table, toml_path("hamiltonian", "dftb", "skf", "format"), format_string)
   end block

Similar like other build interfaces it can be used to create the subtables as well as the string value by providing a default.


Iterating over keys
-------------------

An expressive way to organize data is by providing a table where the keys of each entry describe the object that should be initialized.
For example in a package manager, the keys represent the dependency, where each dependency is declared in a subtable.
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

The first three entries provide a string value, while the fourth entry provides a subtable implicitly by using dotted key-value pairs and the last entry uses an inline table.

Here we want to focus on the iteration and the default initialization, the internal structure of the *requirement_type* is secondary for this example.
We provide the minimal definition only holding the name of the dependency for demonstration purposes.

.. literalinclude:: table/keys/src/requirements.f90
   :caption: src/requirements.f90
   :language: fortran
   :lines: 10-15

For the actual implementation of reading all entries from the table, we will use a one-dimensional array of *requirement_type* values.
Using the ``get_keys`` method of the table we can obtain a list of all keys for the current table, the method will always allocate the ``list`` variable and we can safely allocate the *requirement_type* using the number of keys.
To obtain the subtable, the ``get_value`` interface can be used, it will return a pointer to the subtable, either created implicitly by using a dotted key-value pair or by an inline table as shown in the snippet above.
Finally, we can call the actual constructor of the *requirement_type* using the subtable references with the ``child`` pointer.

.. literalinclude:: table/keys/src/requirements.f90
   :caption: src/requirements.f90
   :language: fortran
   :lines: 19-66

The other scenario we want to support is the presence of a string rather than a subtable.
In this case, the ``get_value`` interface will fail, while it provides an optional status argument to check for successful operation, we can more conveniently and idiomatically verify the success by checking the state of the ``child`` pointer.
If there is no subtable to reference, *i.e.* because it is a key-value pair with a string entry, the ``child`` pointer will not be associated, which can be easily checked.
For this case we will again use the ``get_value`` interface, but this time to retrieve the entry into a deferred length character.
Again we can idiomatically check the status of the operation using the allocation state of the variable and create the appropriate error message if needed.
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

   The auxiliary module providing the error handler

   .. literalinclude:: table/keys/src/error.f90
      :caption: src/error.f90
      :language: fortran


Array of tables
---------------

A special construct in TOML is the array of tables syntax, it provides a more verbose form to declare several tables in an array, which are usually provided using inline tables as shown below.

.. code-block:: toml

   tasks = [
      {name="optimization", driver="lbfgs"},
      {name="equilibration", driver="velocity-verlet"},
      {name="production", driver="velocity-verlet"},
   ]


Comparing the above example to the snippet below using an array of tables for the *tasks* array, the more verbose form becomes preferable in case further subtables are needed.
Except for the subtables *config* the same data is provided.

.. code-block:: toml

   [[tasks]]
   name = "optimization"
   driver = "lbfgs"
   [task.config]
   tolerance = 1.0e-7

   [[tasks]]
   name = "equilibration"
   driver = "velocity-verlet"
   [task.config]
   time-step = 1.0
   temperature = 300.0
   max-steps = 500

   [[tasks]]
   name = "production"
   driver = "velocity-verlet"
   [task.config]
   time-step = 1.0
   temperature = 300.0
   max-steps = 10000


To represent this data we can use a single *task_config* derived type with a polymorphic *driver_config* member identifying the actual task.
For this example, we will have two implementations of such tasks such as LBFGS and Velocity Verlet, which are defined in the following snippets.

.. literalinclude:: table/aot/src/task.f90
   :caption: src/task.f90
   :language: fortran
   :lines: 9-35

To read the array of tables we start from the root document and fetch the *tasks* entry as an array using the ``get_value`` interface.
The length of the full arrays is known and we can use it to allocate the list of *task_config* values before reading the individual entries.
The individual tables inside the array can be addressed using the ``get_value`` interface by passing the (one-based) index.

.. literalinclude:: table/aot/src/task.f90
   :caption: src/task.f90
   :language: fortran
   :lines: 39-57

.. note::

   In the setup above, if the *tasks* entry is not present it will be implicitly created as an empty array.
   The allocation and the loop over the entries will work, however the consuming code should check whether no tasks are meaningful or should produce an error.

To read the individual tasks we define a separate procedure to make it easily reusable and hide the fact that we are working with a subtable.
To make the task *name* optional we make it default to the driver name, for *allocatable* or *pointer* variables the exit status of ``get_value`` can be easily checked by the allocation or association status of the respective variable, alternatively an integer variable can be passed to the optional *stat* argument.
Finally, the configuration reader is called depending on the value of *driver* for ease of usage we use a block construct to allocate the specific type and then transfer it using *move_alloc* into the *task_config*.

.. literalinclude:: table/aot/src/task.f90
   :caption: src/task.f90
   :language: fortran
   :lines: 59-95

For reading the actual driver configuration we use the ``get_value`` interface to obtain the settings.
We use the same defaulting mechanism as for the *name* entry here.

.. literalinclude:: table/aot/src/task.f90
   :caption: src/task.f90
   :language: fortran
   :lines: 97-117

Note that this example does not propagate back errors but directly calls *error stop*, for a more robust error reporting this can be changed by a small error handle or a context type.

.. dropdown:: Full source code

   The full module implementing the *task_config* reading

   .. literalinclude:: table/aot/src/task.f90
      :caption: src/task.f90
      :language: fortran
