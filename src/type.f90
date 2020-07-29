! This file is part of toml-f.
!
! Copyright (C) 2019-2020 Sebastian Ehlert
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

!> Collection of the central datatypes to define TOML data structures
!
!  All TOML data types should inherit from an abstract value allowing to generate
!  a generic interface to deal with all more specialized TOML data types, while
!  the abstract value is interesting for developing algorithms in TOML-Fortran,
!  the user of TOML-Fortran will usually only care about TOML tables.
!
!  The TOML types defined here should implement the TOML data structures (mostly)
!  without taking the actual implementation of the data structures into account.
!  This is done by providing a bare minimum interface using type bound procedures
!  to minimize the interdependencies between the datatypes.
!
!  To make the data types extendable a visitor pattern allows access to the TOML
!  data types and can be used to implement further algorithms.
module tomlf_type
   use tomlf_type_array, only : toml_array, new_array, new, len
   use tomlf_type_keyval, only : toml_keyval, new_keyval, new
   use tomlf_type_table, only : toml_table, new_table, new
   use tomlf_type_value, only : toml_value, toml_visitor, toml_key
   implicit none
   private

   public :: toml_value, toml_visitor, toml_table, toml_array, toml_keyval
   public :: toml_key
   public :: new, new_table, new_array, new_keyval, len


end module tomlf_type
