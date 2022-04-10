:sd_hide_title: true

TOML Fortran library
====================

.. image:: _static/header.svg
   :alt: TOML Fortran
   :width: 100%

.. image:: https://img.shields.io/badge/license-MIT%7CApache%202.0-blue
   :alt: License
   :target: https://opensource.org/licenses/Apache-2.0

.. image:: https://img.shields.io/github/v/release/toml-f/toml-f
   :alt: Version
   :target: https://github.com/toml-f/toml-f/releases/latest

.. image:: https://github.com/toml-f/toml-f/workflows/CI/badge.svg
   :alt: Continuous Integration
   :target: https://github.com/toml-f/toml-f/actions

.. image:: https://github.com/toml-f/toml-f/workflows/docs/badge.svg
   :alt: API docs
   :target: https://toml-f.github.io/toml-f

.. image:: https://codecov.io/gh/toml-f/toml-f/branch/master/graph/badge.svg
   :alt: Coverage
   :target: https://codecov.io/gh/toml-f/toml-f


This project provides a library to work with `TOML <https://toml.io>`_ files and data structures and allows to define data serialization and deserialization in Fortran.
The currently supported TOML standard is `version 1.0.0 <https://toml.io/en/v1.0.0>`__.

A typical package manifest for Fortran package manager (`fpm <https://fpm.fortran-lang.org>`_) specified in TOML is shown below

.. literalinclude:: ../fpm.toml
   :language: toml
   :caption: fpm.toml


.. toctree::

   Tutorials <tutorial/index>
   How-Tos <how-to/index>
   Reference <https://toml-f.github.io/toml-f>
