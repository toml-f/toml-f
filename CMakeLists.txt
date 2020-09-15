# This file is part of toml-f.
#
# Copyright (C) 2019-2020 Sebastian Ehlert
#
# Licensed under either of Apache License, Version 2.0 or MIT license
# at your option; you may not use this file except in compliance with
# the License.
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

cmake_minimum_required(VERSION 3.9)
get_directory_property(is-subproject PARENT_DIRECTORY)

project(
  "toml-f"
  LANGUAGES "Fortran"
  VERSION "0.2.0"
)

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

# General configuration information
add_subdirectory("config")

# Collect source of the project
set(srcs)
add_subdirectory("src")

# TOML-Fortran library target
add_library(
  "${PROJECT_NAME}-lib"
  "${srcs}"
)
set_target_properties(
  "${PROJECT_NAME}-lib"
  PROPERTIES
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
)
target_include_directories(
  "${PROJECT_NAME}-lib"
  INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

if(NOT is-subproject OR BUILD_SHARED_LIBS)
  # Export targets for other projects
  install(
    TARGETS
    "${PROJECT_NAME}-lib"
    EXPORT
    "${PROJECT_NAME}-config"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  )

  install(
    EXPORT "${PROJECT_NAME}-config"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  )

  # Package license files
  install(
    FILES
    "LICENSE-Apache"
    "LICENSE-MIT"
    DESTINATION "${CMAKE_INSTALL_DATADIR}/licenses/${PROJECT_NAME}"
  )
else()
  # Expose license files to parent project
  set(
    "${PROJECT_NAME}-lic"
    "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE-Apache"
    "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE-MIT"
    PARENT_SCOPE
  )
endif()

# add the testsuite
enable_testing()
set(fpm-toml "${CMAKE_CURRENT_SOURCE_DIR}/fpm.toml")
add_subdirectory("test")
