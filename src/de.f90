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

!> Proxy module for providing loading and deserialization of TOML data structures
module tomlf_de
   use tomlf_constants, only : tfc, TOML_NEWLINE
   use tomlf_de_context, only : toml_context
   use tomlf_de_lexer, only : toml_lexer, new_lexer_from_string, new_lexer_from_unit, &
      & new_lexer_from_file
   use tomlf_de_parser, only : parse, toml_parser_config
   use tomlf_diagnostic, only : toml_level
   use tomlf_error, only : toml_error
   use tomlf_type, only : toml_table
   implicit none
   private

   public :: toml_parse
   public :: toml_load, toml_loads
   public :: toml_context, toml_parser_config, toml_level


   !> Parse a TOML document.
   !>
   !> This interface is deprecated in favor of [[toml_load]] and [[toml_loads]]
   interface toml_parse
      module procedure :: toml_parse_unit
      module procedure :: toml_parse_string
   end interface toml_parse

   !> Load a TOML data structure from the provided source
   interface toml_load
      module procedure :: toml_load_file
      module procedure :: toml_load_unit
   end interface toml_load

   !> Load a TOML data structure from a string
   interface toml_loads
      module procedure :: toml_load_string
   end interface toml_loads


contains


!> Parse a TOML input from a given IO unit.
!>
!> @note This procedure is deprectated
subroutine toml_parse_unit(table, unit, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   !> Unit to read from
   integer, intent(in) :: unit
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   call toml_load(table, unit, error=error)
end subroutine toml_parse_unit

!> Wrapper to parse a TOML string.
!>
!> @note This procedure is deprectated
subroutine toml_parse_string(table, string, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   !> String containing TOML document
   character(len=*), intent(in), target :: string
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   call toml_loads(table, string, error=error)
end subroutine toml_parse_string

!> Load TOML data structure from file
subroutine toml_load_file(table, filename, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   character(*, tfc), intent(in) :: filename
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(toml_lexer) :: lexer
   type(toml_error), allocatable :: error_

   call new_lexer_from_file(lexer, filename, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, config, context, error)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine toml_load_file

!> Load TOML data structure from unit
subroutine toml_load_unit(table, io, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   !> Unit to read from
   integer, intent(in) :: io
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(toml_lexer) :: lexer
   type(toml_error), allocatable :: error_

   call new_lexer_from_unit(lexer, io, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, config, context, error)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine toml_load_unit

!> Load TOML data structure from string
subroutine toml_load_string(table, string, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   !> String containing TOML document
   character(*, tfc), intent(in) :: string
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(toml_lexer) :: lexer

   call new_lexer_from_string(lexer, string)
   call parse(lexer, table, config, context, error)
end subroutine toml_load_string

end module tomlf_de
