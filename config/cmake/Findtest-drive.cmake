# This file is part of toml-f.
# SPDX-Identifier: Apache-2.0 OR MIT
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

set(_lib "test-drive")
set(_pkg "TEST_DRIVE")
set(_url "https://github.com/fortran-lang/test-drive")
set(_rev "v0.4.0")

if(NOT DEFINED "${_pkg}_FIND_METHOD")
  if(DEFINED "${PROJECT_NAME}-dependency-method")
    set("${_pkg}_FIND_METHOD" "${${PROJECT_NAME}-dependency-method}")
  else()
    set("${_pkg}_FIND_METHOD" "cmake" "pkgconf" "subproject" "fetch")
  endif()
  set("_${_pkg}_FIND_METHOD")
endif()

foreach(method in ITEMS ${${_pkg}_FIND_METHOD})
  if(TARGET "${_lib}::${_lib}")
    break()
  endif()

  if("${method}" STREQUAL "cmake")
    message(STATUS "${_lib}: Find installed package")
    find_package("${_lib}" CONFIG)
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
    set("${_pkg}_SOURCE_DIR" "${PROJECT_SOURCE_DIR}/subprojects/${_lib}")
    set("${_pkg}_BINARY_DIR" "${PROJECT_BINARY_DIR}/subprojects/${_lib}")
    if(EXISTS "${${_pkg}_SOURCE_DIR}/CMakeLists.txt")
      message(STATUS "Include ${_lib} from subprojects")
      add_subdirectory(
        "${${_pkg}_SOURCE_DIR}"
        "${${_pkg}_BINARY_DIR}"
        )

      add_library("${_lib}::${_lib}" INTERFACE IMPORTED)
      target_link_libraries("${_lib}::${_lib}" INTERFACE "${_lib}")

      # We need the module directory in the subproject before we finish the configure stage
      if(NOT EXISTS "${${_pkg}_BINARY_DIR}/include")
        make_directory("${${_pkg}_BINARY_DIR}/include")
      endif()

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

    # We need the module directory in the subproject before we finish the configure stage
    FetchContent_GetProperties("${_lib}" BINARY_DIR "${_pkg}_BINARY_DIR")
    if(NOT EXISTS "${${_pkg}_BINARY_DIR}/include")
      make_directory("${${_pkg}_BINARY_DIR}/include")
    endif()

    break()
  endif()

endforeach()

if(NOT TARGET "${_lib}::${_lib}")
  message(FATAL_ERROR "Could not find dependency ${_lib}")
endif()

if(DEFINED "_${_pkg}_FIND_METHOD")
  unset("${_pkg}_FIND_METHOD")
  unset("_${_pkg}_FIND_METHOD")
endif()
unset(_lib)
unset(_pkg)
unset(_url)
unset(_rev)
