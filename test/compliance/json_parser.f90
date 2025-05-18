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

module tjson_parser
   use tomlf_constants, only : tfc, tfi, tfr, toml_type
   use tomlf_datetime, only : toml_datetime
   use tomlf_de_context, only : toml_context
   use tjson_lexer, only : json_lexer, new_lexer_from_string, new_lexer_from_unit, &
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
subroutine json_load_file(object, filename, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   class(toml_value), allocatable, intent(out) :: object
   !> Name of the file to load
   character(*, tfc), intent(in) :: filename
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(json_lexer) :: lexer
   type(toml_error), allocatable :: error_
   type(toml_table), allocatable :: table

   call new_lexer_from_file(lexer, filename, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, config, context, error)
      if (allocated(table)) call prune(object, table)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine json_load_file

!> Load TOML data structure from unit
subroutine json_load_unit(object, io, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   class(toml_value), allocatable, intent(out) :: object
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
   type(toml_table), allocatable :: table

   call new_lexer_from_unit(lexer, io, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, config, context, error)
      if (allocated(table)) call prune(object, table)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine json_load_unit

!> Load TOML data structure from string
subroutine json_load_string(object, string, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   class(toml_value), allocatable, intent(out) :: object
   !> String containing TOML document
   character(*, tfc), intent(in) :: string
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(json_lexer) :: lexer
   type(toml_table), allocatable :: table

   call new_lexer_from_string(lexer, string)
   call parse(lexer, table, config, context, error)
   if (allocated(table)) call prune(object, table)
end subroutine json_load_string

!> Prune the artificial root table inserted by the lexer
subroutine prune(object, table)
   !> Instance of the TOML data structure, not allocated in case of error
   class(toml_value), allocatable, intent(inout) :: object
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(inout) :: table

   type(json_prune) :: pruner

   call table%pop("_", object)

   if (allocated(object)) call object%accept(pruner)
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
         dval = toml_datetime(sval)
         call val%set(dval)
      case("bool")
         call val%set(sval == "true")
      case("integer")
         read(sval, *, iostat=stat) ival
         if (stat == 0) then
            call val%set(ival)
         end if
      case("float")
         read(sval, *, iostat=stat) fval
         if (stat == 0) then
            call val%set(fval)
         end if
      end select
   end select
end subroutine prune_value

end module tjson_parser
