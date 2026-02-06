.. _integration:

Using TOML Fortran
==================

This tutorial shows how to integrate the TOML Fortran library with your build system and use it easily in your project.


Using the Fortran package manager
---------------------------------

The Fortran package manager (`fpm <https://fpm.fortran-lang.org>`_) is a tool for building Fortran projects and managing dependencies on Fortran libraries.
To enable TOML Fortran in your fpm project add the following entry to your package manifest:

.. code-block:: toml
   :caption: fpm.toml

   [dependencies]
   toml-f.git = "https://github.com/toml-f/toml-f"

When building your project fpm will automatically fetch TOML Fortran for you and build it as part of your project.
The TOML Fortran modules become useable in your project.


Integrate with meson
--------------------

To allow meson to use TOML Fortran it is easiest to include it as subproject using a git wrap file placed in the ``subprojects`` directory.

.. code-block:: ini
   :caption: subprojects/toml-f.wrap

   [wrap-git]
   directory = toml-f
   url = https://github.com/toml-f/toml-f
   revision = head

The revision can be adjusted to pin a specific release tag of TOML Fortran for additional stability.
In the projects meson buid file the dependency method can be used to access TOML Fortran using the wrap file to define a fallback which is built on-demand.

.. code-block:: meson
   :caption: meson.build

   # Create TOML Fortran as subproject
   tomlf_dep = dependency(
     'toml-f',
     version: '>=0.2.0',
     fallback: ['toml-f', 'tomlf_dep'],
     default_options: ['default_library=static'],
   )

Finally, you can add ``tomlf_dep`` as dependency to any of your targets and are done.


Integrate with CMake
--------------------

To use TOML Fortran in CMake based projects it is useful to define your own find-module, to allow on-demand compilation of TOML Fortran as well as discovery of installed packages from both meson and CMake based builds.

.. code-block:: text

   .
   ├── CMakeLists.txt
   ├── cmake
   │   └── Findtoml-f.cmake
   :

In your main CMake build file you have to include the custom find-module in your ``CMAKE_MODULE_PATH``, afterwards you can just use ``find_package`` to obtain the ``toml-f::toml-f`` target and link against it.

.. code-block:: cmake
   :caption: CMakeLists.txt

   # ...

   # Make custom find modules available for project
   list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
   set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" PARENT_SCOPE)

   # Find TOML Fortran, check whether target is already available,
   # e.g. when build as subproject of another project also using TOML Fortran
   if(NOT TARGET "toml-f::toml-f")
     find_package(toml-f REQUIRED)
   endif()

   # ...

   # Add library target
   library(
     "${PROJECT_NAME}"
   )

   # Link against TOML Fortran library
   target_link_libraries(
     "${PROJECT_NAME}"
     PUBLIC
     "toml-f::toml-f"
   )

   # ...

   # Follow GNU conventions for installation destinations
   include(GNUInstallDirs)

   # Make custom find-modules available when installing project
   install(
     DIRECTORY
     "${CMAKE_CURRENT_SOURCE_DIR}/cmake/"
     DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
   )

Note that we also install the find-modules, this is important if you want to make your CMake projects reusable in the same way TOML Fortran can be used in your project.
Finally we need some boilerplate to define the custom find-module is documented below.

Imported Targets
^^^^^^^^^^^^^^^^

This module provides the following imported target, if found:

``toml-f::toml-f``
  The toml-f library


Result Variables
^^^^^^^^^^^^^^^^

This module will define the following variables:

``TOML_FORTRAN_FOUND``
  True if the toml-f library is available

``TOML_FORTRAN_SOURCE_DIR``
  Path to the source directory of the toml-f project,
  only set if the project is included as source.

``TOML_FORTRAN_BINARY_DIR``
  Path to the binary directory of the toml-f project,
  only set if the project is included as source.

Cache variables
^^^^^^^^^^^^^^^

The following cache variables may be set to influence the library detection:

``TOML_FORTRAN_FIND_METHOD``
  Methods to find or make the project available. Available methods are

  - ``cmake``: Try to find via CMake config file
  - ``pkgconf``: Try to find via pkg-config file
  - ``subproject``: Use source in subprojects directory
  - ``fetch``: Fetch the source from upstream

``TOML_FORTRAN_DIR``
  Used for searching the CMake config file

``TOML_FORTRAN_SUBPROJECT``
  Directory to find the toml-f subproject, relative to the project root

.. code-block:: cmake
   :caption: cmake/Findtoml-f.cmake

   set(_lib "toml-f")
   set(_pkg "TOML_FORTRAN")
   set(_url "https://github.com/toml-f/toml-f")
   set(_rev "HEAD")

   if(NOT DEFINED "${_pkg}_FIND_METHOD")
     if(DEFINED "${PROJECT_NAME}-dependency-method")
       set("${_pkg}_FIND_METHOD" "${${PROJECT_NAME}-dependency-method}")
     else()
       set("${_pkg}_FIND_METHOD" "cmake" "pkgconf" "subproject" "fetch")
     endif()
     set("_${_pkg}_FIND_METHOD")
   endif()

   foreach(method ${${_pkg}_FIND_METHOD})
     if(TARGET "${_lib}::${_lib}")
       break()
     endif()

     if("${method}" STREQUAL "cmake")
       message(STATUS "${_lib}: Find installed package")
       if(DEFINED "${_pkg}_DIR")
         set("_${_pkg}_DIR")
         set("${_lib}_DIR" "${_pkg}_DIR")
       endif()
       find_package("${_lib}" CONFIG QUIET)
       if("${_lib}_FOUND")
         message(STATUS "${_lib}: Found installed package")
         break()
       endif()
     endif()

     if("${method}" STREQUAL "pkgconf")
       find_package(PkgConfig QUIET)
       pkg_check_modules("${_pkg}" QUIET "${_lib}")
       if("${_pkg}_FOUND")
         message(STATUS "Found ${_lib} via pkg-config")

         add_library("${_lib}::${_lib}" INTERFACE IMPORTED)
         target_link_libraries(
           "${_lib}::${_lib}"
           INTERFACE
           "${${_pkg}_LINK_LIBRARIES}"
           )
         target_include_directories(
           "${_lib}::${_lib}"
           INTERFACE
           "${${_pkg}_INCLUDE_DIRS}"
           )

         break()
       endif()
     endif()

     if("${method}" STREQUAL "subproject")
       if(NOT DEFINED "${_pkg}_SUBPROJECT")
         set("_${_pkg}_SUBPROJECT")
         set("${_pkg}_SUBPROJECT" "subprojects/${_lib}")
       endif()
       set("${_pkg}_SOURCE_DIR" "${PROJECT_SOURCE_DIR}/${${_pkg}_SUBPROJECT}")
       set("${_pkg}_BINARY_DIR" "${PROJECT_BINARY_DIR}/${${_pkg}_SUBPROJECT}")
       if(EXISTS "${${_pkg}_SOURCE_DIR}/CMakeLists.txt")
         message(STATUS "Include ${_lib} from ${${_pkg}_SUBPROJECT}")
         add_subdirectory(
           "${${_pkg}_SOURCE_DIR}"
           "${${_pkg}_BINARY_DIR}"
         )

         add_library("${_lib}::${_lib}" INTERFACE IMPORTED)
         target_link_libraries("${_lib}::${_lib}" INTERFACE "${_lib}")
         break()
       endif()
     endif()

     if("${method}" STREQUAL "fetch")
       message(STATUS "Retrieving ${_lib} from ${_url}")
       include(FetchContent)
       FetchContent_Declare(
         "${_lib}"
         GIT_REPOSITORY "${_url}"
         GIT_TAG "${_rev}"
         )
       FetchContent_MakeAvailable("${_lib}")

       add_library("${_lib}::${_lib}" INTERFACE IMPORTED)
       target_link_libraries("${_lib}::${_lib}" INTERFACE "${_lib}")

       FetchContent_GetProperties("${_lib}" SOURCE_DIR "${_pkg}_SOURCE_DIR")
       FetchContent_GetProperties("${_lib}" BINARY_DIR "${_pkg}_BINARY_DIR")
       break()
     endif()

   endforeach()

   if(TARGET "${_lib}::${_lib}")
     set("${_pkg}_FOUND" TRUE)
   else()
     set("${_pkg}_FOUND" FALSE)
   endif()

   if(DEFINED "_${_pkg}_SUBPROJECT")
     unset("${_pkg}_SUBPROJECT")
     unset("_${_pkg}_SUBPROJECT")
   endif()
   if(DEFINED "_${_pkg}_DIR")
     unset("${_lib}_DIR")
     unset("_${_pkg}_DIR")
   endif()
   if(DEFINED "_${_pkg}_FIND_METHOD")
     unset("${_pkg}_FIND_METHOD")
     unset("_${_pkg}_FIND_METHOD")
   endif()
   unset(_lib)
   unset(_pkg)
   unset(_url)
   unset(_rev)


Other build systems
-------------------

Other build systems must discover a precompiled TOML Fortran library from the system.
For this purpose the ``pkg-config`` tool is used.
After installing TOML Fortran with either meson or CMake a pc-file is generated which can be discovered by ``pkg-config`` and describes how to compile against the installed module files as well as link against the TOML Fortran library.
First check if the ``pkg-config`` tool is available and can discover TOML Fortran

.. code-block:: text

   pkg-config --modversion toml-f

Make sure to adjust the ``PKG_CONFIG_PATH`` environment variable to point to the correct installation directory.
Using the ``--libs`` and ``--cflags`` options the libraries to link against as well as the include directories can be obtained:

.. code-block:: text

   pkg-config --cflags toml-f
   pkg-config --libs toml-f

In a handwritten Makefile those can be included by

.. code-block:: make

   TOML_FORTRAN_INCLUDE_FLAGS := $(shell pkg-config --cflags toml-f)
   TOML_FORTRAN_LIBRARY_FLAGS := $(shell pkg-config --libs toml-f)
