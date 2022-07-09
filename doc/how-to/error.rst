Reporting errors
================

TOML data structures can record their origin in the original TOML document, which can be used reporting errors with rich context information.
The recipes here describe how to obtain the context for producing error messages and diagnostics using the origin information of the data structures.


Loading with rich context
-------------------------

To make use of the origin information, the context from loading the document has to be preserved.
This can be archived by passing the optional ``context`` argument to the loading interface to request the document context to be exported.
We define a simple data type for a configuration at

.. literalinclude:: error/single/src/config.f90
   :language: fortran
   :caption: src/config.f90
   :lines: 6-10

To report errors, we now not only use the TOML data structure, but also a context object, which allows to create the report.

.. literalinclude:: error/single/src/config.f90
   :language: fortran
   :caption: src/config.f90
   :lines: 17-34, 46

To obtain the context object we have to request it when reading the TOML document.

.. literalinclude:: error/single/app/main.f90
   :language: fortran
   :caption: app/main.f90

Now we can try with an incorrect configuration file, where we pass a string instead of a float to our option.

.. literalinclude:: error/single/example1.toml
   :language: toml
   :caption: config.toml

The error message is produced and shows the exact origin of the value in the document.

.. ansi-block::

   error: Cannot read timestep
    --> config.toml:1:12-18
     |
   1 | timestep = "large"
     |            ^^^^^^^ expected real value
     |

Now we also have to handle the case where the value can be read correctly, but is incorrect for our application, like a negative timestep.

.. literalinclude:: error/single/src/config.f90
   :language: fortran
   :caption: src/config.f90
   :lines: 35-39

The origin information will still be available and allow us to make a rich report about the error in the input.

.. literalinclude:: error/single/example2.toml
   :language: toml
   :caption: config.toml

The resulting error message is shown below.

.. ansi-block::

   error: Timestep must be positive
    --> fpm.toml:1:12-15
     |
   1 | timestep = -0.1
     |            ^^^^ expected positive real value
     |

.. note::

   Each TOML data structure has an *origin* attribute, which can be used together with the report function of the context.
   In case the origin cannot be mapped to a single token, *e.g.* for the root table, the value of the origin will be zero.
   The report function will only produce labels for non-zero origins and gracefully ignore data without origin in the current context.

The reporting function is not limited to errors, it can also produce warnings or informational messages.
For this purpose we select the appropriate ``toml_level`` for the report.

.. literalinclude:: error/single/src/config.f90
   :language: fortran
   :caption: src/config.f90
   :lines: 41-44

.. tip::

   The ``toml_level`` parameter provides a statically initialized derived type enumerating all available report levels.
   You can think of it as an enumerator with a proper namespace.

We can test this for the following example.

.. literalinclude:: error/single/example3.toml
   :language: toml
   :caption: config.toml

The resulting warning is shown below.

.. ansi-block::

   warning: Large timesteps can lead to instable simulations
    --> config.toml:1:12-16
     |
   1 | timestep = 100.0
     |            ^^^^^
     |

.. dropdown:: full source

   The full *demo_config* module is given here.

   .. literalinclude:: error/single/src/config.f90
      :language: fortran
      :caption: src/dependency.f90

   The driver for running the examples is given below.

   .. literalinclude:: error/single/app/main.f90
      :language: fortran
      :caption: app/main.f90


Multiline reports
-----------------

In come cases multiple labels are required to express the context of the report correctly.
This feature is available with the context object, by providing the origin of the two data structures in the reporting function.

An example for this is the dependency table in fpm, where we can either provide a local dependency using the *path* key or a remote dependency using the *git* key, but not both at the same time.

We declare a simple dummy dependency storing only the dependency name for demonstration purposes.

.. literalinclude:: error/multi/src/dependency.f90
   :language: fortran
   :caption: src/dependency.f90
   :lines: 6-10

We iterate over the list of all subtables in the dependency table and to read the actual dependency.
In case an entry is not a subtable we will raise an error, since a package manifest can contain multiple dependency tables, we will report which table we are currently in as additional context.

.. literalinclude:: error/multi/src/dependency.f90
   :language: fortran
   :caption: src/dependency.f90
   :lines: 14-39

An example triggering the error is shown below.

.. literalinclude:: error/multi/example1.toml
   :language: toml
   :caption: fpm.toml

Running this example will produce the following error showing line 1 and line 3 of our example input.

.. ansi-block::

   error: All entries must be subtables
    --> fpm.toml:3:1-6
     |
   1 | [dependencies]
     |  ------------ required for this table
     :
   3 | toml-f = "^0.3.0"
     | ^^^^^^ must be a subtable
     |

Now we want to implement the actual conflicting case described above.
Here we just read the two strings from the *git* and *path* entry.
Note that the *get_value* interface will not allocate the string if no value is present, which allows to conveniently check for success via allocation status of the strings.

.. literalinclude:: error/multi/src/dependency.f90
   :language: fortran
   :caption: src/dependency.f90
   :lines: 41-75

To preserve the order from the input we can compare the *origin* values of the two retrieved strings and produce the appropriate error message.

In this example, the *git* entry was defined first and a conflicting *path* entry is provided afterwards.

.. literalinclude:: error/multi/example2.toml
   :language: toml
   :caption: fpm.toml

The order is reported correctly in the produced error message shown below.

.. ansi-block::

   error: Remote dependency cannot have local path
    --> fpm.toml:3:15-36
     |
   2 | toml-f.git = "https://github.com/toml-f/toml-f.git"
     |              -------------------------------------- remote dependency already defined
   3 | toml-f.path = "./subprojects/toml-f"
     |               ^^^^^^^^^^^^^^^^^^^^^^ cannot have local path
     |

The other way round is also possible as shown in this example.

.. literalinclude:: error/multi/example3.toml
   :language: toml
   :caption: fpm.toml

The error message is adjusted accordingly and now reports a conflicting *git* entry to the already defined *path* entry.

.. ansi-block::

   error: Local dependency cannot have remote repository
    --> fpm.toml:3:14-51
     |
   2 | toml-f.path = "./subprojects/toml-f"
     |               ---------------------- local dependency already defined
   3 | toml-f.git = "https://github.com/toml-f/toml-f.git"
     |              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ cannot have remote repository
     |

.. dropdown:: full source

   The full *demo_dependency* module is given provided below.

   .. literalinclude:: error/multi/src/dependency.f90
      :language: fortran
      :caption: src/dependency.f90

   The driver for the examples is given here.

   .. literalinclude:: error/multi/app/main.f90
      :language: fortran
      :caption: app/main.f90
