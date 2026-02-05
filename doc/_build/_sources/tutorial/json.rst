.. _json-lexer:

Writing a custom lexer
======================

.. sectionauthor:: Sebastian Ehlert <@awvwgk>
.. image:: https://img.shields.io/badge/difficulty-intermediate-yellow
   :alt: Difficulty: Intermediate

Many programs already come with their input formats, switching to a different format requires establishing some way to get backward compatibility for older inputs.
When transitioning to TOML Fortran reading of the input will use the provided TOML data structures.
If the previous input format is sufficiently compatible, it can be parsed into a matching TOML data structure and allow to seamlessly use of the TOML format going forward while still providing compatibility for previously written inputs.

This tutorial is meant to teach how lexing in TOML Fortran works and enable the reader to implement their custom lexer for their custom format.
There is no guarantee that a custom input format can be ported by creating a custom lexer, since the format needs to fulfill some basic requirements, like providing typed values.
For this tutorial, we will choose `JSON <https://json.org/>`_ as our input format and walk through all the steps to create a new lexer from scratch.

.. note::

   The choice of JSON for this tutorial is not a coincidence.
   TOML Fortran does implement this lexer to parse JSON files into TOML data structures to support the encoding tests in the validation suite of `BurntSushi/toml-test <https://github.com/BurntSushi/toml-test/tree/v1.1.0#implementing-an-encoder>`_.

.. important::

   This tutorial makes partial use of the internal API of TOML Fortran.


Identifying limitation
----------------------

Before we start to implement our custom lexer, we need to identify any limitations of the TOML data structures to represent our custom format.
TOML documents always have a table at the document root, there is no way to represent a JSON array or single value in TOML.
Furthermore, JSON supports the value type ``null``, which is not representable in TOML.
We have two choices here, either we can flag ``null`` values as an invalid token or we can replace them in the lexer with something else like an empty table.
Finally, there are other details we have to take into account, like how JSON is handling duplicate keys, for most of the implementation-dependent cases we will follow the rules TOML provides.

This tutorial by no means aims for offering a fully compliant parser as we already fail for top-level arrays or ``null`` type values.
For a custom format, this might be even more challenging, especially if the format is defined by only a single implementation.

.. note::

   Writing a compliant JSON parser can quickly become quite challenging (see `Parsing JSON is a Minefield <https://seriot.ch/projects/parsing_json.html>`_).

But format limitations can go both ways, there are of course also features in TOML we cannot express in JSON.
However, since we want to map JSON to TOML and not the other way round we do not have to worry about limitations present in JSON.
Every feature available in TOML representable in the previous input format will be an incentive to switch to the new format.

.. note::

   For the actual application of the JSON parser in the validation suite, this problem is solved by not using only strings to represent values and adding type annotations.
   In TOML Fortran these annotations are mapped back by pruning the read JSON data structure.
   The pruning is done via a visitor which is accepted after the data structure has been completely parsed.


Creating the lexer
------------------

First, we start by creating a new subclass of the abstract base class (ABC) imported from the *tomlf_de_abc* module.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (json_lexer)
   :lines: 15-25,27-53

We start by creating a constructor to consume an input file and turn it into a string to advance through.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (new_lexer_from_file)
   :lines: 60-75

Using a formatted unit is more inefficient compared to reading the whole file with direct access, but needed in case we are dealing with the standard input.
We make sure to error out if we get direct access or stream access units since we cannot reliably read those.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (new_lexer_from_unit)
   :lines: 77-114

Finally, we sometimes also need to read from a string, there we add a constructor which can create a lexer for a string input.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (new_lexer_from_string)
   :lines: 116-124

The parser might need access to some of the internal data of the lexer, which is done via the *get_info* procedure.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (get_info)
   :lines: 126-141


Identifying tokens
------------------

Now that we can instantiate the lexer we need to implement the possibility to recognize tokens, this is done with the *next* method.
We start with creating the actual tokenization step in the *next_token* procedure, which we will call in the *next* method.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (next_token)
   :lines: 162-167

As a first action, we will advance the internal state of the lexer by consuming the last token.
For convenience, we save the position in the source string in the *pos* and *prev* variables.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (next_token, continued)
   :lines: 169-174

The next thing we check is if we have exhausted the input string and if so we return the *end of file* token.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (next_token, continued)
   :lines: 176-180

Now we can inspect the current character from the source string and decide which token it should be labeled.
The character set is quite simple, we have to consider opening and closing brackets and braces, for arrays and tables, respectively, commas, colons, strings, and whitespace.
We will be explicitly producing whitespace tokens here rather than skipping it since the parser can gracefully handle whitespace.
However, we have to consider that newlines have semantical meaning in TOML while they are only considered whitespace in JSON and therefore we will only produce whitespace tokens.

We use a select case statement to decide which token to produce.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (next_token, continued)
   :lines: 182-227

To wrap up the lexing we will try to identify unknown tokens as well as possible trying to advance to the next terminating character.
For the terminating characters, we choose whitespace as well as control characters and place those in the module scope.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (terminated)
   :lines: 55-56

.. note::

   We are cheating a bit here since we declare the colon as an *equal* token.
   This way we can use the same lexer for both JSON and TOML and still have the same parsing rules.

One special case to consider is literals, like strings numbers or booleans.
To not clutter the logic here we create separate routines for parsing the respective literal values.
For obtaining string values we will implement this as *next_string*.
Here we cannot simply advance to the next quote character, since we need to handle escape characters gracefully.
While doing so we can also ensure that the escape sequences found are valid and not malformed.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (next_string)
   :lines: 229-267

Strings can only contain printable characters, therefore we check for valid string characters using a small *valid_string* function for each character.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (valid_string)
   :lines: 377-389

We also need to identify numbers, mapping to either integers or floats in TOML, which is done via *next_number*.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (next_number)
   :lines: 269-330

To support boolean values we implement a *next_boolean* procedure.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (next_boolean)
   :lines: 332-354

Finally, we also want to parse null values using the *next_null* procedure.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (next_null)
   :lines: 356-375

With this logic available we can now generate all required tokens for parsing JSON.

.. tip::

   Moving most of the validation logic in the tokenization simplifies the actual extraction of the value as we have to deal with fewer edge cases.

Now we can wrap up the *next* procedure, instead of directly returning the token we will make some adjustments to the token stream here.
In general, this is the right place to buffer tokens, perform overflow checks, or detect unclosed groups, we will only use it to insert two additional tokens to inject a top-level key.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (next)
   :lines: 143-160

This will direct the parser to leave the root document where newlines are semantically relevant since we cannot produce such newline tokens in our JSON lexer.

.. admonition:: Exercise

   The *nil* token will make the parser skip the respective value.
   If we want to support *null* values, how would we have to modify our lexer to produce for example an empty table ``{}`` instead, *i.e.* a *lbrace* and *rbrace* token?


Extracting values
-----------------

Before we can connect our lexer to the existing TOML parser we have to implement the extraction of the values.
The parser itself will use the *extract* member functions to obtain values for the respective tokens and never directly access the character stream.

To extract the string value we implement the *extract_string* procedure.
We will also use the *extract_string* routine to catch the *keypath* token we inserted in the token stream and return the wanted dummy value.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (extract_string)
   :lines: 391-430

Similarly, we implement the *extract_integer*, instead of using an internal read, we implement the reading ourselves.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (extract_integer)
   :lines: 432-461

For floating point numbers implemented in *extract_float* we will just use an internal read.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (extract_float)
   :lines: 463-479

The last token we can produce and extract from our lexer is are boolean values, which we implement in *extract_boolean*.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (extract_boolean)
   :lines: 481-493

We create a mocked routine for *extract_datetime* since we cannot produce this token in JSON.

.. literalinclude:: ../../test/compliance/json_lexer.f90
   :language: fortran
   :caption: src/json_lexer.f90 (extract_datetime)
   :lines: 495-506

This provides our lexer with full functionality regarding the extraction of values needed for parsing and creating data structures.

.. dropdown:: full source

   For completeness here is again the full source of our lexer implementation.

   .. literalinclude:: ../../test/compliance/json_lexer.f90
      :language: fortran
      :caption: src/json_lexer.f90
      :lines: 14-


Verifying the lexer
-------------------

We could start right into connecting our lexer with the parser, but we have not yet verified that the tokenization and value extraction work as expected.
For this purpose, we will create some unit tests using the `test-drive <https://github.com/fortran-lang/test-drive>`_ framework.

As the entry point for our tester, we will use the standard wrapper for launching test suites.

.. dropdown:: tester program

   Taken from the `test-drive`_ README

   .. literalinclude:: json/test/main.f90
      :language: fortran
      :caption: test/main.f90
      :lines: 14-

Our actual test suite for the lexer will be based on a routine called *check_token*, which creates a new lexer from a string and retrieves all tokens while comparing them with a reference token stream.
We then can implement our checks by providing a string and a list of tokens to see whether our lexer can identify the expected tokens correctly.
For visualization, we use the *tomlf_diagnostic* module to label the tokens in the actual source string.

.. literalinclude:: json/test/test_lexer.f90
   :language: fortran
   :caption: test/test_lexer.f90
   :lines: 14-29,30,33,40,42,45,48,50,59-70,219-234,243-251,274-281,301-308,143-150,203-210,380-

These are only a couple of tests, we have much more cases to consider for a robust lexer.

.. admonition:: Exercise

   Write at least ten more tests for edge cases in the lexer.
   Make sure to include invalid cases and ensure that even invalid tokens are generated correctly.


Connecting to the parser
------------------------

Now that we have verified the tokenization process in our lexer we can connect our custom lexer to the default TOML parser.

For this purpose we define convenience interfaces called *json_load* / *json_loads* similar to the available *toml_load* / *toml_loads* interfaces.
Other than the TOML-related load interfaces, we will also use them to implement necessary post-processing steps for the data structure.

.. literalinclude:: ../../test/compliance/json_parser.f90
   :language: fortran
   :caption: src/json_parser.f90
   :lines: 14-42

The *json_load* interface is implemented by *json_load_file* and *json_load_unit*.
The former is a wrapper that is using the *new_lexer_from_file* constructor.

.. literalinclude:: ../../test/compliance/json_parser.f90
   :language: fortran
   :caption: src/json_parser.f90 (json_load_file)
   :lines: 53-77

The latter wraps the *new_lexer_from_unit* constructor.

.. literalinclude:: ../../test/compliance/json_parser.f90
   :language: fortran
   :caption: src/json_parser.f90 (json_load_unit)
   :lines: 79-103

Finally, we also provide *json_loads* by implementing *json_load_string* using our *new_lexer_from_string* constructor.

.. literalinclude:: ../../test/compliance/json_parser.f90
   :language: fortran
   :caption: src/json_parser.f90 (json_load_unit)
   :lines: 105-124

These wrappers so far are very straightforward, first setting up a lexer instance and invoking the *parse* procedure which will construct the actual parser instance and process the token stream.
After a successful run, the *table* instance will be allocated, for the post-processing, we invoke the *prune* routine.

.. literalinclude:: ../../test/compliance/json_parser.f90
   :language: fortran
   :caption: src/json_parser.f90 (prune)
   :lines: 127-131,134-135,138

Where we effectively remove the first child from the root table and return is a polymorphic *toml_value*.
This has the advantage that we can support arrays and values at the root level with our JSON loader.

.. tip::

   The user can dispatch the value using a *select type* construct or by creating a view using the *cast_to_table* / *cast_to_array* / *cast_to_keyval* functions.


.. dropdown:: full source

   For completeness here is again the full source of our parser implementation.

   Note that this implementation also contains an implementation of a *toml_visitor* to prune type annotations used in the validation test suite to represent TOML values.

   .. literalinclude:: ../../test/compliance/json_parser.f90
      :language: fortran
      :caption: src/json_parser.f90
      :lines: 14-


Summary
-------

Now we have a working lexer that can tokenize JSON documents into TOML parsable tokens.
The lexer implemented in TOML Fortran works on a similar construction, with the difference that the TOML grammar is much more complicated to parse than JSON.

.. important::

   In this tutorial, you have learned about the tokenization process used in TOML Fortran.
   You can now

   - implement a custom lexer based on the TOML tokens
   - verify your lexer against an expected token stream
   - adjust the token stream to direct the parsing process
   - add a post-processing step to prune the resulting data structure
