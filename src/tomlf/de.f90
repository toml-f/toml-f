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

module tomlf_de
   use tomlf_constants, only : TOML_NEWLINE
   use tomlf_de_lexer, only : toml_lexer, new_lexer_from_string, new_lexer_from_unit
   use tomlf_de_parser, only : parse
   use tomlf_error, only : toml_error, io_error
   use tomlf_type, only : toml_table
   implicit none
   private

   public :: toml_parse


   interface toml_parse
      module procedure :: toml_parse_unit
      module procedure :: toml_parse_string
   end interface toml_parse


contains


!> Parse a TOML input from a given IO unit.
subroutine toml_parse_unit(table, unit, error)
   type(toml_table), allocatable, intent(out) :: table
   integer, intent(in) :: unit
   type(toml_error), allocatable, intent(out), optional :: error

   type(toml_lexer) :: lexer
   type(toml_error), allocatable :: error_

   call new_lexer_from_unit(lexer, unit, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, error)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine toml_parse_unit


!> Wrapper to parse a TOML string.
subroutine toml_parse_string(table, conf, error)
   type(toml_table), allocatable, intent(out) :: table
   character(len=*), intent(in), target :: conf
   type(toml_error), allocatable, intent(out), optional :: error

   type(toml_lexer) :: lexer

   call new_lexer_from_string(lexer, conf)
   call parse(lexer, table, error)
end subroutine toml_parse_string


end module tomlf_de
