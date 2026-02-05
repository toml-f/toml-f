Building a linter
=================

.. sectionauthor:: Sebastian Ehlert <@awvwgk>
.. image:: https://img.shields.io/badge/difficulty-beginner-brightgreen
   :alt: Difficulty: Beginner

This tutorial will show how to use TOML Fortran to build a linter for your configuration files.
Linters provide a way to encourage or enforce a certain style or flag up common usage errors.


Target selection
----------------

This tutorial will look into finding lint in the package manifest from the Fortran package manager (`fpm <https://fpm.fortran-lang.org>`_).
We will use its plugin mechanism to create a new subcommand called ``lint``.

We start with setting up the package manifest for our linter:

.. code-block:: toml
   :caption: fpm.toml

   name = "fpm-lint"
   version = "0.1.0"

   [dependencies]
   toml-f.git = "https://github.com/toml-lang/toml-f.git"


Configuration of the linter
---------------------------

To configure our linter we will use the `extra section <https://fpm.fortran-lang.org/en/spec/manifest.html#additional-free-data-field>`__ in the manifest which is specially reserved for tools integrating with fpm and boldly claim *extra.fpm.lint* as our configuration section.
Using the package manifest provides us with two advantages, first this document will be present in all projects using fpm, second if we can read our configuration from the manifest, we are already sure it is valid TOML.

.. code-block:: toml
   :caption: fpm.toml

   # ...
   [extra.fpm.lint]
   package-name = true
   bare-keys = true

Now we will set up our main program to run the linter.

.. literalinclude:: fpm-lint/app/main0.f90
   :language: fortran
   :caption: app/main.f90

We create a utility module for the *get_argument* function used to retrieve the manifest name, in most cases we can default to *fpm.toml*, but for testing it is convenient to pass an argument.

.. literalinclude:: fpm-lint/src/utils.f90
   :language: fortran
   :caption: src/utils.f90
   :lines: 1-5, 7, 13-15, 49-69

The first error source we can encounter stems from parsing the TOML document itself.
This is outside of our responsibility to handle, still we want to check whether we can report the error correctly.

.. literalinclude:: fpm-lint/example/0-invalid.toml
   :language: toml
   :caption: fpm.toml (invalid)

Running the linter on this document will break with the following message produced by the *toml_load* procedure.

.. ansi-block::

   ❯ fpm run -- invalid.toml
   [0;1;31merror[0m[0;1m: Invalid expression for value[0m
    [0;1;34m-->[0m invalid.toml:4:15
     [0;1;34m|[0m
   4 [0;1;34m|[0m package-name =
     [0;1;34m|[0m               [0;1;31m^[0m [0;1;31munexpected newline[0m
     [0;1;34m|[0m

With this case covered we proceed with reading the configuration for our linter.

Our configuration from the package manifest will be stored in a *lint_config* type which we define in a separate module.
Reading the configuration will happen from the root table, meaning we have to advance through several subtables first before we can process the options for our linter.
We want to report errors with rich context information here as well, therefore we request the *origin* in every call to the *get_value* interface and produce a report using the *context* we obtained in the main program.

.. literalinclude:: fpm-lint/src/config.f90
   :language: fortran
   :caption: src/config.f90

For convenience, we defined a *make_error* routine to allocate the error handler and store our report from the context.
At this point, we should check whether our error reporting works and run the linter on an incorrect TOML document.

.. literalinclude:: fpm-lint/example/0-incorrect.toml
   :language: toml
   :caption: fpm.toml

.. dropdown:: current main program

   Putting everything together in the main program should look like this.

   .. literalinclude:: fpm-lint/app/main1.f90
      :language: fortran
      :caption: app/main.f90

Running our linter on this file will correctly flag this as an error since a string value is provided rather than a boolean value.

.. ansi-block::

   ❯ fpm run -- fpm.toml
   [0;1;31merror[0m[0;1m: Entry in 'package-name' must be boolean[0m
    [0;1;34m-->[0m fpm.toml:4:16-21
     [0;1;34m|[0m
   4 [0;1;34m|[0m package-name = "true"
     [0;1;34m|[0m                [0;1;31m^^^^^^[0m [0;1;31mexpected boolean value[0m
     [0;1;34m|[0m

Finally, we define a logging mechanism to capture our actual linting messages which are not fatal.
The logger provides two procedures, *add_message* to store a message and *show_log* to display all stored messages.

.. literalinclude:: fpm-lint/src/logger.f90
   :language: fortran
   :caption: src/logger.f90


Recommended package name
------------------------

As a first linting check we will inspect the package name, for this we will apply the following rules:

1. the package name should be a TOML bare key to not require quotes in *dependency* sections, characters like dots, colons, or slashes are not allowed
2. TOML generally favors lowercase dashed keys, therefore we will discourage capitalization (camelCase and PascalCase) as well as underscores (snake_case)
3. there are several ways to declare strings in TOML, we want to favor the normal string one

An example of a package name we would disallow would be *fpmLinter* as seen in the manifest below.

.. literalinclude:: fpm-lint/example/1-camel-case.toml
   :language: toml
   :caption: fpm.toml

Let's start with our implementation of this check.
For convenience we will reexport the other modules from the *fpm_lint* module, this allows one clean import in the main program.
Then we define the *lint_data* procedure, where we first check whether the *name* key is present, if not we create a message at the *info* level and leave our block scope, as all further checks rely on the presence of the entry.

We can now check whether the entry is provided as a string or maybe as something else, like a literal string, which we can flag.
Furthermore, we verify that the package name uses only lowercase letters, numbers, and dashes with the *verify* intrinsic.

.. literalinclude:: fpm-lint/src/lint.f90
   :language: fortran
   :caption: src/lint.f90
   :lines: 1-14, 16-66, 191

.. tip::

   The ``toml_level`` parameter provides a statically initialized derived type enumerating all available report levels.
   Similarly, the ``token_kind`` parameter provides an enumeration of the token kinds.
   You can think of it as an enumerator with a proper namespace.

.. dropdown:: current main program

   Putting everything together in the main program should look like this.

   .. literalinclude:: fpm-lint/app/main2.f90
      :language: fortran
      :caption: app/main.f90

We check this on the camelCase package name from above and can find the following output.

.. ansi-block::

   ❯ fpm run -- fpm.toml
   [0;1;35minfo[0m[0;1m: Package name should be lowercase with dashes[0m
    [0;1;34m-->[0m fpm.toml:1:8-18
     [0;1;34m|[0m
   1 [0;1;34m|[0m name = "fpmLinter"
     [0;1;34m|[0m        [0;1;35m^^^^^^^^^^^[0m
     [0;1;34m|[0m


.. admonition:: Exercise
   :class: note

   Add a check for the length of the package name, everything under three characters is probably a bad choice, so is a too long package name.

   Create an example to trigger the error with your new check.
   What happens if a too long camelCase package name is used?


Bare key paths preferred
------------------------

TOML allows to quote keys, however this might become visually distracting if some keys are quoted and others are not.
With our package name rule, there should not be the need to quote any keys even in dependency sections.

To determine whether a string is used in the context of a key we need a way to identify all keys.
We could check all entries in the data structures by implementing a visitor object which walks through all tables and checks the keys.
However, this is somewhat inefficient and we can also miss keys that are not recorded.

.. literalinclude:: fpm-lint/example/2-dotted-keys.toml
   :language: toml
   :caption: fpm.toml

In this example, the second occurrence of the key ``toml-f`` will only reference the table but it is already defined the line before.
The quotation marks are visually identifiable as lint and we need a programmatic way to flag this.

Instead of working with the data structure, we will use the parser to record more tokens in the context.
Rather than using the context to only report errors, we will use it to identify keys.
This is done by increasing the *context_detail* option in the *config* keyword of the parser to one.
Now all tokens except for whitespace and comments will be recorded.

.. code-block:: fortran
   :caption: app/main.f90

   call toml_load(table, manifest, error=error, context=context, &
     & config=toml_parser_config(color=color, context_detail=1))

.. tip::

   Increasing the ``context_detail`` to two will also record whitespace and comments.
   This can be useful when writing checks for whitespace or indentation styles.

Our linter pass will work as follows:

1. identifying all relevant keys in the manifest
2. check whether they are keypath tokens
3. create a report for any key that is a string or a literal

Our implementation reflects this by first collecting an array of *toml_key* objects in *list* and then iterating over all entries checking whether they are the correct *token_kind*.

.. literalinclude:: fpm-lint/src/lint.f90
   :language: fortran
   :caption: src/lint.f90
   :lines: 67-96

To create the list we need to implement the *identify_keys* procedure.
The rules in TOML for key paths are simple: before an equal sign we can have key paths and keypath can only be present in table bodies or inline tables.
This can be implemented by using a stack storing whether the current scope belongs in a table, array, or value.

We will always push a new scope on the respective token opening it, *i.e.* a value is opened by an equal sign, an array by a right bracket, and an inline table by a right curly brace.
To distinguish table headers from inline arrays we only push arrays on our stack after an equal sign.
Finally, we default to a table scope if no other scope is present and we have collected all required rules to identify key paths.
Similarly, we can identify the endings of the scopes.

We then can check whether the current scope on the top of the stack allows key paths and record those in our list.

.. literalinclude:: fpm-lint/src/lint.f90
   :language: fortran
   :caption: src/lint.f90
   :lines: 98-189

For convenience, we implement a *push_back* and *pop* function to add and remove scopes from our stack.
The *pop* function will additionally perform a check whether we want to remove a matching scope and save us some repetition in the loop this way.

In our utility module, we implement the *resize* procedure for an array of integers

.. literalinclude:: fpm-lint/src/utils.f90
   :language: fortran
   :caption: src/utils.f90
   :lines: 1-48, 69

.. dropdown:: current main program

   Putting everything together in the main program should look like this.

   .. literalinclude:: fpm-lint/app/main3.f90
      :language: fortran
      :caption: app/main.f90

At this point, we can now add a call in our main program to run the linter.

.. ansi-block::

   ❯ fpm run -- fpm.toml
   [0;1;35minfo[0m[0;1m: String used in key path[0m
    [0;1;34m-->[0m fpm.toml:5:1-8
     [0;1;34m|[0m
   5 [0;1;34m|[0m "toml-f".tag = "v0.2.3"
     [0;1;34m|[0m [0;1;35m^^^^^^^^[0m [0;1;35muse bare key instead[0m
     [0;1;34m|[0m

Now for something more tricky with an inline table to check whether our scoping rules are working correctly.

.. literalinclude:: fpm-lint/example/2-inline-table.toml
   :language: toml
   :caption: fpm.toml

Our linter can correctly identify the *tag* entry as a string in the key path context and produces the appropriate message.

.. ansi-block::

   ❯ fpm run -- fpm.toml
   [0;1;35minfo[0m[0;1m: String used in key path[0m
    [0;1;34m-->[0m fpm.toml:4:53-57
     [0;1;34m|[0m
   4 [0;1;34m|[0m toml-f = {git = "https://github.com/toml-f/toml-f", "tag" = "v0.2.3"}
     [0;1;34m|[0m                                                     [0;1;35m^^^^^[0m [0;1;35muse bare key instead[0m
     [0;1;34m|[0m


.. admonition:: Exercise
   :class: note

   Previously, we flagged the usage of a literal string as a value for the package name, however a package manifest can contain much more string values.

   Create a check for all string values in the manifest to ensure they use double-quotes.
   Collect string values (*string*, *literal*, *mstring*, and *mliteral*) from array and value scopes for this purpose.

   Can you make a meaningful suggestion if a literal string contains characters that must be escaped in a double-quoted string?


Summary
-------

This concludes the linting we wanted to implement for the fpm package manifest.
For a feature-complete linter, the rule set to check for is usually growing with time and might also shift as new rules are adopted.
Our linter currently provides only a few rules but has the potential to include more checks as the need arises.

.. admonition:: Exercise

   Our output is currently in the order of the checks, rather than in the order of reports occurring in the TOML document.
   The output of the reports might become more intuitive if it was sorted according to the source lines.

   Record the first character in the output together with the messages in the logger.
   Have the logger sort the messages according to their order before printing them.

.. important::

   In this tutorial, you have learned how to report custom error messages in your TOML input data.
   You can now

   - report colorized error messages with rich context information
   - create error messages when reading a TOML data structure
   - control the details captured in the context describing the TOML document
   - check a TOML document based on the token information in the context
