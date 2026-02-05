:author: Sebastian Ehlert
:date: 2022-08-03
:category: release
:excerpt: 1

Jonquil: Bringing TOML blooms to JSON land
==========================================

.. figure:: ../_static/img/jonquil.svg
   :alt: Jonquil Blossom
   :align: center
   :width: 50%

Jonquil started out of the idea to make a TOML parser speak JSON.
First explored as a way to connect to the toml-test validation suite, the implementation was brief and expressive enough to be explored as a tutorial.
Since the implementation was taken serious enough to enter into JSON Fortran's benchmarks and turned out to actually be competitive with JSON Fortran, revisiting the idea of a JSON parser in TOML Fortran was warranted.

Jonquil aims to provide a compatibility layer to allow seamless usage of both TOML and JSON in the same project.
Based on the TOML Fortran API a JSON lexer is implemented to connect the JSON grammar with the existing TOML parser.
Just a parser however does not make a library, Jonquil provides a flavor of the public TOML Fortran API for consistency, retaining full compatibility with TOML Fortran even on the ABI level.
Everything you can do with TOML Fortran is also possible with Jonquil, and they can be mixed and matched.


References
----------

- `Jonquil repository <https://github.com/toml-f/jonquil>`__
- `JSON Fortran repository <https://github.com/jacobwilliams/json-fortran>`__
- `rojff repository <https://github.com/everythingfunctional/rojff>`__
- `toml-test JSON encoding <https://github.com/burntsushi/toml-test#json-encoding>`__
- `JSON Fortran's parser benchmark <https://github.com/jacobwilliams/json-fortran-benchmarks>`__
- Tutorial: :ref:`json-lexer`
- Recipe: :ref:`jonquil`
