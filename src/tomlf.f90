! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Public API for TOML Fortran
!>
!> This module provides the main entry point to the TOML Fortran library.
!> It re-exports all public types and procedures needed for parsing, manipulating,
!> and serializing TOML documents.
!>
!> ## Parsing TOML
!>
!> Use [[toml_load]] to load a TOML document from a file or unit, or [[toml_loads]]
!> to parse a TOML string directly:
!>
!>```fortran
!> type(toml_table), allocatable :: table
!> call toml_load(table, "config.toml")
!>```
!>
!> ## Accessing Values
!>
!> Use [[get_value]] to retrieve values from tables and arrays, and [[set_value]]
!> to modify or create new values:
!>
!>```fortran
!> character(len=:), allocatable :: name
!> call get_value(table, "name", name)
!>```
!>
!> ## Serialization
!>
!> Use [[toml_dump]] to write a table to a file or [[toml_dumps]] to serialize
!> to a string:
!>
!>```fortran
!> character(len=:), allocatable :: output
!> call toml_dumps(table, output)
!>```
module tomlf
   use tomlf_build, only : get_value, set_value, toml_path
   use tomlf_datetime, only : toml_datetime, to_string
   use tomlf_de, only : toml_parse, toml_load, toml_loads, &
      & toml_context, toml_parser_config, toml_level
   use tomlf_error, only : toml_error, toml_stat
   use tomlf_ser, only : toml_serializer, toml_serialize, toml_dump, toml_dumps
   use tomlf_terminal, only : toml_terminal
   use tomlf_type, only : toml_table, toml_array, toml_keyval, toml_key, toml_value, &
      & is_array_of_tables, new_table, add_table, add_array, add_keyval, len
   use tomlf_utils_sort, only : sort
   use tomlf_version, only : tomlf_version_string, tomlf_version_compact, &
      & get_tomlf_version
   implicit none
   public

end module tomlf
