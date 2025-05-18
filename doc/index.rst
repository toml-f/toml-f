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

.. image:: https://readthedocs.org/projects/toml-f/badge/?version=latest
   :target: https://toml-f.readthedocs.io
   :alt: Documentation Status

.. image:: https://codecov.io/gh/toml-f/toml-f/branch/main/graph/badge.svg
   :alt: Coverage
   :target: https://codecov.io/gh/toml-f/toml-f


This project provides a library to work with `TOML <https://toml.io>`_ files and data structures and allows to define data serialization and deserialization in Fortran.
The currently supported TOML standard is `version 1.0.0 <https://toml.io/en/v1.0.0>`__.
The TOML Fortran project is hosted at GitHub at `toml-f/toml-f <https://github.com/toml-f/toml-f>`__.

.. grid:: 3
   :gutter: 3

   .. grid-item-card::
      :class-card: sd-bg-light sd-rounded-3
      :class-header: sd-text-primary sd-text-center sd-font-weight-bold sd-border-bottom-0
      :link: tutorial
      :link-type: ref
      :shadow: none

      :octicon:`mortar-board` Tutorials
      ^^^
      Guides and courses for using TOML with complete and self-contained examples.

   .. grid-item-card::
      :class-card: sd-bg-light sd-rounded-3
      :class-header: sd-text-primary sd-text-center sd-font-weight-bold sd-border-bottom-0
      :link: recipe
      :link-type: ref
      :shadow: none

      :octicon:`book` Recipes
      ^^^
      Examples and recipes for solving common tasks with TOML Fortran.

   .. grid-item-card::
      :class-card: sd-bg-light sd-rounded-3
      :class-header: sd-text-primary sd-text-center sd-font-weight-bold sd-border-bottom-0
      :link: https://toml-f.github.io/toml-f
      :shadow: none

      :octicon:`gear` Reference
      ^^^
      Generated documentation of all procedures and derived types available.

   .. grid-item-card::
      :class-card: sd-bg-light sd-rounded-3
      :class-header: sd-text-primary sd-text-center sd-font-weight-bold sd-border-bottom-0
      :link: installation
      :link-type: ref
      :shadow: none

      :octicon:`arrow-down` Installation
      ^^^
      Instructions for installing, updating or compiling TOML Fortran.

   .. grid-item-card::
      :class-card: sd-bg-light sd-rounded-3
      :class-header: sd-text-primary sd-text-center sd-font-weight-bold sd-border-bottom-0
      :link: https://github.com/toml-f/toml-f
      :shadow: none

      :octicon:`mark-github` Repository
      ^^^
      GitHub repository for the development of TOML Fortran.

   .. grid-item-card::
      :class-card: sd-bg-light sd-rounded-3
      :class-header: sd-text-primary sd-text-center sd-font-weight-bold sd-border-bottom-0
      :link: external
      :link-type: ref
      :shadow: none

      :octicon:`link` External
      ^^^
      External links to other resources, blogs, etc.


.. card::
   :class-body: sd-fs-4 sd-font-weight-bold
   :class-card: sd-mt-5 sd-px-5 sd-py-1 sd-rounded-3 sd-bg-light sd-text-center
   :shadow: none

   :octicon:`pin`
   News

.. postlist:: 5
   :excerpts:


.. card::
   :class-body: sd-fs-4 sd-font-weight-bold
   :class-card: sd-mt-5 sd-px-5 sd-py-1 sd-rounded-3 sd-bg-light sd-text-center
   :shadow: none

   :octicon:`rocket`
   Users


TOML Fortran is used for example in the Fortran package manager (`fpm <https://fpm.fortran-lang.org>`_), a typical package manifest specified in TOML is shown below

.. literalinclude:: ../fpm.toml
   :language: toml
   :caption: fpm.toml

A collection of projects using TOML Fortran is shown below:

.. grid:: 2

   .. grid-item::

      .. figure:: https://opengraph.githubassets.com/x/fortran-lang/fpm
         :alt: GitHub Preview
         :target: https://github.com/fortran-lang/fpm

      Fortran package manager and build system.

   .. grid-item::

      .. figure:: https://opengraph.githubassets.com/x/tblite/tblite
         :alt: GitHub Preview
         :target: https://github.com/tblite/tblite

      Light-weight tight-binding framework.

   .. grid-item::

      .. figure:: https://opengraph.githubassets.com/x/popelier-group/FEREBUS-v7
         :alt: GitHub Preview
         :target: https://github.com/popelier-group/FEREBUS-v7

      Gaussian Process Regression engine written in Fortran90 designed to produce models for atomistic simulations.

   .. grid-item::

      .. figure:: https://opengraph.githubassets.com/x/lewisfish/signedMCRT
         :alt: GitHub Preview
         :target: https://github.com/lewisfish/signedMCRT

      Signed distance fields in Monte Carlo Radiative Transfer for modelling of smooth surfaces without the need to use triangle or similar meshes.

Your project is using TOML Fortran but not listed here?
Let us know of your work by opening a `discussion on GitHub <https://github.com/orgs/toml-f/discussions>`__ or create a `pull request <https://github.com/toml-f/toml-f/blob/main/CONTRIBUTING.md>`__ to add it to the selection above.


.. toctree::
   :maxdepth: 2
   :hidden:

   Tutorials <tutorial/index>
   How-Tos <how-to/index>
   References <reference/index>
   News <news/index>
   external
