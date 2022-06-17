.. _installation:

Installing TOML Fortran
=======================

This guide will walk you through installing the latest version of TOML Fortran.
If you know your way around fpm, CMake or meson, checkout the :ref:`integration guide <integration>` to allow on-demand compilation of TOML Fortran as well as discovery of installed libraries.


:fab:`apple` :fab:`linux` :fab:`windows` Installing from conda-forge
--------------------------------------------------------------------

.. image:: https://img.shields.io/conda/vn/conda-forge/toml-f
   :alt: Conda
   :target: https://github.com/conda-forge/toml-f-feedstock

.. image:: https://img.shields.io/conda/pn/conda-forge/toml-f
   :alt: Conda
   :target: https://github.com/conda-forge/toml-f-feedstock


This project is packaged for the *mamba* package manager and available on the *conda-forge* channel.
To install the *mamba* package manager we recommend the `mambaforge <https://github.com/conda-forge/miniforge/releases>`_ installer.
If the *conda-forge* channel is not yet enabled, add it to your channels with

.. code-block:: bash

   mamba config --add channels conda-forge
   mamba config --set channel_priority strict

Once the *conda-forge* channel has been enabled, TOML Fortran can be installed with *mamba*:

.. code-block:: shell

   mamba install toml-f

It is possible to list all of the versions of TOML Fortran available on your platform with *mamba*:

.. code-block:: shell

   mamba repoquery search toml-f --channel conda-forge


:fab:`freebsd` FreeBSD ports
----------------------------

.. image:: https://repology.org/badge/version-for-repo/freebsd/toml-f.svg
   :alt: FreeBSD
   :target: https://www.freshports.org/textproc/toml-f/

A port for FreeBSD is available

.. code-block:: bash

   pkg install textproc/toml-f

In case no package is available build the port using

.. code-block:: bash

   cd /usr/ports/textproc/toml-f
   make install clean

For more information see the `toml-f port details <https://www.freshports.org/textproc/toml-f/>`_.


:fab:`apple` :fab:`linux` Building with spack
---------------------------------------------

.. image:: https://repology.org/badge/version-for-repo/spack/toml-f.svg
   :alt: Spack
   :target: https://github.com/spack/spack/blob/develop/var/spack/repos/builtin/packages/toml-f/package.py

This project is available with the `spack <https://spack.io>`_ package manager.
You can install the TOML Fortran package with

.. code-block:: text

   spack install toml-f
   spack load toml-f

To depend on TOML Fortran in your `spack`_ package you can add a dependency with

.. code-block:: python

   depends_on("toml-f")


:fab:`linux` Using EasyBuild
----------------------------

TOML Fortran is available with `EasyBuild <https://docs.easybuild.io/en/latest/index.html>`_.
You can check the available configurations using the search option

.. code-block:: text

   ❯ eb -S TOML-Fortran
   CFGS1=.../easyconfigs/t/TOML-Fortran
    * $CFGS1/TOML-Fortran-0.2.2-GCC-10.2.0.eb
    * $CFGS1/TOML-Fortran-0.2.2-iccifort-2020.4.304.eb

Select one matching configuration and build TOML Fortran using

.. code-block:: text

   ❯ eb TOML-Fortran-0.2.2-GCC-10.2.0.eb -r
   == temporary log file in case of crash /tmp/eb-51Jk58/easybuild-NI5Ee1.log
   == resolving dependencies ...
   == processing EasyBuild easyconfig TOML-Fortran-0.2.2-GCC-10.2.0.eb
   == building and installing TOML-Fortran-0.2.2-GCC-10.2.0.eb...
   [...]
   == Build succeeded for 1 out of 1
   == Temporary log file(s) /tmp/eb-51Jk58/easybuild-NI5Ee1.log* have been removed.
   == Temporary directory /tmp/eb-51Jk58 has been removed.

TOML Fortran should now be available via an environment module.
For more details checkout the `EasyBuild`_ documentation.


:fab:`apple` Using Homebrew
---------------------------

TOML Fortran is available in a custom tap at `grimme-lab/homebrew-qc <https://github.com/grimme-lab/homebrew-qc>`__ for the `brew <https://brew.sh>`_ package manager.
You can include the tap by using

.. code-block:: text

   brew tap grimme-lab/qc
   brew install toml-f

To build with a custom Fortran compiler you can set the ``FC`` environment variable and force to ignore preexisting binary distributions from the tap

.. code-block:: text

   export FC=gfortran-11
   brew install -s toml-f

.. note::

   TOML Fortran has not yet been submitted to `homebrew-core <https://github.com/homebrew/homebrew-core>`_.


Building from source
--------------------

To build this project from the source code in this repository you need to have

- a Fortran compiler supporting Fortran 2008

  - GFortran 5 or newer
  - Intel Fortran 18 or newer
  - NAG 7 or newer

- One of the supported build systems

  - `meson <https://mesonbuild.com>`_ version 0.55 or newer
  - `CMake <https://cmake.org/>`_ version 3.9 or newer

First, get the source by cloning the repository

.. code-block:: bash

   git clone https://github.com/toml-f/toml-f
   cd toml-f


Using Meson
^^^^^^^^^^^

To build this project with meson a build-system backend is required, *i.e.* `ninja <https://ninja-build.org>`_ version 1.7 or newer.
Setup a build with

.. code-block:: bash

   meson setup _build --prefix=/path/to/installation

You can select the Fortran compiler by the ``FC`` environment variable.
To compile the project run

.. code-block:: bash

   meson compile -C _build

We employ a `validator suite <https://github.com/BurntSushi/toml-test>`_ to test the standard compliance of this implementation.
To use this testing a *go* installation is required.
The installation of the validator suite will be handled by meson automatically without installing into the users *go* workspace.
Run the tests with

.. code-block:: bash

   meson test -C _build --print-errorlogs

To run the full decoder test add the benchmark argument.
This test will currently fail, due to the implementation not yet supporting Unicode escape sequences.

.. code-block:: bash

   meson test -C _build --benchmark --print-errorlogs

The binary used for transcribing the TOML documents to the testing format is ``_build/test/toml2json`` and can be used to check on per test basis.
Finally, you can install TOML Fortran with

.. code-block:: bash

   meson install -C _build


Using CMake
^^^^^^^^^^^

While meson is the preferred way to build this project it also offers CMake support.
Configure the CMake build with

.. code-block:: bash

   cmake -B_build -GNinja -DCMAKE_INSTALL_PREFIX=/path/to/installation

Similar to meson the compiler can be selected with the ``FC`` environment variable.
You can build the project using

.. code-block:: bash

   cmake --build _build

To include *toml-f* in your CMake project, check the [example integration with CMake](https://github.com/toml-f/tf-cmake-example).
The validation suite is currently not supported as unit test for CMake builds and requires a manual setup instead using the *toml2json* binary.
Finally, you can install TOML Fortran with

.. code-block:: bash

   cmake --install _build


Supported compilers
-------------------

This is a non-comprehensive list of tested compilers for TOML Fortran.
Compilers with the label *latest* are tested with continuous integration for each commit.

========== =========================== ==================== ============== ===============
 Compiler   Version                     Platform             Architecture   version
========== =========================== ==================== ============== ===============
 GCC        11.1, 10.3, 9.4, 8.5, 7.5   Ubuntu 20.04         x86_64         0.2.3, latest
 GCC        9.4, 6.5                    MacOS 10.15.7        x86_64         0.2.3, latest
 GCC        11.0                        MacOS 11.0           arm64          0.2.3
 GCC        9.4                         CentOS 7             ppc64le        0.2.3
 GCC        9.4                         CentOS 7             aarch64        0.2.3
 GCC/MinGW  8.1                         Window Server 2019   x86_64         0.2.3, latest
 GCC/MinGW  5.3                         Window Server 2019   x86_64         0.2.3
 Intel      2022.0                      Ubuntu 20.04         x86_64         0.2.3, latest
 Intel      19                          OpenSUSE             x86_64         0.2.3
 NAG        7.1                         RHEL                 x86_64         0.2.3
========== =========================== ==================== ============== ===============

Compiler known to fail are documented here, together with the last commit where this behaviour was encountered.
If available an issue in on the projects issue tracker or the issue tracker of the dependencies is linked.
Usually, it safe to assume that older versions of the same compiler will fail to compile as well and this failure is consistent over platforms and/or architectures.

========== ============= =============== ============== ==========================
 Compiler   Version       Platform        Architecture   Reference
========== ============= =============== ============== ==========================
 Flang      20190329      Ubuntu 20.04    x86_64         `f066ec6`_, `toml-f#28`_
 NVHPC      20.9          Manjaro Linux   x86_64         `f066ec6`_, `toml-f#27`_
========== ============= =============== ============== ==========================

.. _f066ec6: https://github.com/toml-f/toml-f/tree/f066ec6e7fb96d8faf83ab6614ee664a26ad8d57
.. _toml-f#28: https://github.com/toml-f/toml-f/issues/28
.. _toml-f#27: https://github.com/toml-f/toml-f/issues/27
