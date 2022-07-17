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

module tftest_json_parser
   use tomlf_constants, only : tfc, tfi, tfr, TOML_NEWLINE, TOML_BACKSPACE, TOML_TABULATOR, &
      & TOML_FORMFEED, TOML_CARRIAGE_RETURN, toml_type
   use tomlf_datetime, only : toml_datetime
   use tomlf_de_context, only : toml_context
   use tftest_json_lexer, only : json_lexer, new_lexer_from_string, new_lexer_from_unit, &
      & new_lexer_from_file
   use tomlf_de_parser, only : parse, toml_parser_config
   use tomlf_diagnostic, only : toml_level
   use tomlf_build, only : get_value
   use tomlf_error, only : toml_error
   use tomlf_type, only : toml_table, toml_value, cast_to_table, &
      & toml_visitor, toml_array, toml_keyval, toml_key, len
   implicit none
   private

   public :: json_load, json_loads
   public :: toml_context, toml_parser_config, toml_level


   !> Load a TOML data structure from the provided source
   interface json_load
      module procedure :: json_load_file
      module procedure :: json_load_unit
   end interface json_load

   !> Load a TOML data structure from a string
   interface json_loads
      module procedure :: json_load_string
   end interface json_loads

   !> Implement pruning of annotated values as visitor
   type, extends(toml_visitor) :: json_prune
   contains
      !> Traverse the AST and prune all annotated values
      procedure :: visit
   end type json_prune


contains

!> Load TOML data structure from file
subroutine json_load_file(table, filename, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   character(*, tfc), intent(in) :: filename
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(json_lexer) :: lexer
   type(toml_error), allocatable :: error_

   call new_lexer_from_file(lexer, filename, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, config, context, error)
      if (allocated(table)) call prune(table)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine json_load_file

!> Load TOML data structure from unit
subroutine json_load_unit(table, io, config, context, error)
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

   type(json_lexer) :: lexer
   type(toml_error), allocatable :: error_

   call new_lexer_from_unit(lexer, io, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, config, context, error)
      if (allocated(table)) call prune(table)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine json_load_unit

!> Load TOML data structure from string
subroutine json_load_string(table, string, config, context, error)
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

   type(json_lexer) :: lexer

   call new_lexer_from_string(lexer, string)
   call parse(lexer, table, config, context, error)
   if (allocated(table)) call prune(table)
end subroutine json_load_string

!> Prune the artificial root table inserted by the lexer
subroutine prune(table)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(inout) :: table

   type(toml_table), allocatable :: root
   type(toml_table), pointer :: ptr
   class(toml_value), pointer :: child
   type(json_prune) :: pruner

   call move_alloc(table, root)
   call root%get("_", child)

   ptr => cast_to_table(child)
   if (associated(ptr)) table = ptr

   if (allocated(table)) call table%accept(pruner)
end subroutine prune

!> Visit a TOML value
subroutine visit(self, val)
   !> Instance of the JSON pruner
   class(json_prune), intent(inout) :: self
   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   select type(val)
   class is(toml_array)
      call visit_array(self, val)
   class is(toml_table)
      call visit_table(self, val)
   end select
end subroutine visit

!> Visit a TOML array
subroutine visit_array(visitor, array)
   !> Instance of the JSON pruner
   class(json_prune), intent(inout) :: visitor
   !> TOML value to visit
   type(toml_array), intent(inout) :: array

   class(toml_value), allocatable :: val, tmp
   character(kind=tfc, len=:), allocatable :: str
   type(toml_key), allocatable :: vt(:)
   integer :: i, n, stat

   n = len(array)
   do i = 1, n
      call array%shift(val)
      select type(val)
      class default
         call val%accept(visitor)
      class is(toml_table)
         call val%get_keys(vt)
         if (val%has_key("type") .and. val%has_key("value") .and. size(vt)==2) then
            call get_value(val, "type", str)
            call prune_value(tmp, val, str)
            call val%destroy
            call tmp%accept(visitor)
            call array%push_back(tmp, stat)
            cycle
         else
            call val%accept(visitor)
         end if
      end select
      call array%push_back(val, stat)
   end do
end subroutine visit_array

!> Visit a TOML table
subroutine visit_table(visitor, table)
   !> Instance of the JSON pruner
   class(json_prune), intent(inout) :: visitor
   !> TOML table to visit
   type(toml_table), intent(inout) :: table

   class(toml_value), pointer :: ptr
   class(toml_value), allocatable :: val
   character(kind=tfc, len=:), allocatable :: str
   type(toml_key), allocatable :: list(:), vt(:)
   integer :: i, n, stat

   call table%get_keys(list)
   n = size(list, 1)

   do i = 1, n
      call table%get(list(i)%key, ptr)
      select type(ptr)
      class default
         call ptr%accept(visitor)
      class is(toml_table)
         call ptr%get_keys(vt)
         if (ptr%has_key("type") .and. ptr%has_key("value") .and. size(vt)==2) then
            call get_value(ptr, "type", str)
            call prune_value(val, ptr, str)
            call val%accept(visitor)
            call table%delete(list(i)%key)
            call table%push_back(val, stat)
         else
            call ptr%accept(visitor)
         end if
      end select
   end do
end subroutine visit_table

subroutine prune_value(val, table, str)
   !> Actual TOML value
   class(toml_value), allocatable, intent(out) :: val
   !> TOML table to prune
   type(toml_table), intent(inout) :: table
   !> Value kind
   character(kind=tfc, len=*), intent(in) :: str

   class(toml_value), pointer :: ptr
   character(:, tfc), pointer :: sval
   character(kind=tfc, len=:), allocatable :: tmp
   integer :: stat
   type(toml_datetime) :: dval
   integer(tfi) :: ival
   real(tfr) :: fval

   call table%get("value", ptr)
   allocate(val, source=ptr)
   if (allocated(table%key)) then
      val%key = table%key
   else
      deallocate(val%key)
   end if

   select type(val)
   class is(toml_keyval)
      call val%get(sval)
      select case(str)
      case("date", "time", "datetime", "date-local", "time-local", "datetime-local")
         val%raw = sval
         dval = toml_datetime(sval)
         call val%set(dval)
      case("bool")
         val%raw = sval
         call val%set(sval == "true")
      case("integer")
         val%raw = sval
         read(sval, *, iostat=stat) ival
         if (stat == 0) then
            call val%set(ival)
         end if
      case("float")
         val%raw = sval
         read(sval, *, iostat=stat) fval
         if (stat == 0) then
            call val%set(fval)
         end if
      end select
   end select
end subroutine prune_value

!> Transform a TOML raw value to a JSON compatible escaped string
subroutine unquote_json(raw, string)
   !> JSON compatible escaped string
   character(len=*), intent(in) :: string
   !> Raw value of TOML value
   character(len=:), allocatable, intent(out) :: raw

   integer :: i
   logical :: escape

   raw = ''
   escape = .false.
   do i = 2, len(string)-1
      if (escape) then
         select case(string(i:i))
         case default; raw = raw // string(i:i)
         case('\'); raw = raw // '\'
         case('"'); raw = raw // '"'
         case('n'); raw = raw // TOML_NEWLINE
         case('f'); raw = raw // TOML_FORMFEED
         case('r'); raw = raw // TOML_CARRIAGE_RETURN
         case('t'); raw = raw // TOML_TABULATOR
         case('b'); raw = raw // TOML_BACKSPACE
         end select
         escape = .false.
      else
         select case(string(i:i))
         case default; raw = raw // string(i:i)
         case('\'); escape = .true.
         end select
      end if
   end do
end subroutine unquote_json

end module tftest_json_parser
