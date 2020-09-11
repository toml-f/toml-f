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

project(
  "toml-f"
  LANGUAGES "Fortran"
  VERSION "0.2.0"
)
enable_testing()

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

# General configuration information
add_subdirectory("config")

set(srcs)
add_subdirectory("src")

set(
  "${PROJECT_NAME}-mod"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-mod"
)

add_library(
  "${PROJECT_NAME}-lib"
  "${srcs}"
)
set_target_properties(
  "${PROJECT_NAME}-lib"
  PROPERTIES
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
  Fortran_MODULE_DIRECTORY "${PROJECT_NAME}-mod"
)

install(
  TARGETS "${PROJECT_NAME}-lib"
  LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
  INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)
