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

!> Implementation of a serializer for TOML values to JSON, used for testing only.
module tftest_json_ser
   use tomlf_constants
   use tomlf_datetime
   use tomlf_type, only : toml_value, toml_visitor, toml_key, toml_table, &
      & toml_array, toml_keyval, is_array_of_tables, len
   use tomlf_utils, only : convert_raw, toml_get_value_type
   implicit none
   private

   public :: json_serializer


   !> Serializer to produduce a JSON document from a TOML datastructure
   type, extends(toml_visitor) :: json_serializer

      !> Unit for output
      integer :: unit = tfout

      !> Indentation
      character(len=:), allocatable :: indentation

      !> Current depth in the tree
      integer :: depth = 0

   contains

      !> Visit a TOML value
      procedure :: visit

   end type json_serializer


contains


!> Visit a TOML value
subroutine visit(self, val)

   !> Instance of the JSON serializer
   class(json_serializer), intent(inout) :: self

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   select type(val)
   class is(toml_keyval)
      call visit_keyval(self, val)
   class is(toml_array)
      call visit_array(self, val)
   class is(toml_table)
      call visit_table(self, val)
   end select

end subroutine visit


!> Visit a TOML key-value pair
subroutine visit_keyval(visitor, keyval)

   !> Instance of the JSON serializer
   class(json_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_keyval), intent(inout) :: keyval

   character(kind=tfc, len=:), allocatable :: str
   character(kind=tfc, len=:), allocatable :: key
   type(toml_datetime) :: ts
   integer(tfi) :: idummy
   real(tfr) :: fdummy
   logical :: stat, ldummy

   call indent(visitor)

   if (allocated(keyval%key)) then
      call escape_string(keyval%key, key)
      write(visitor%unit, '("""", a, """: ")', advance='no') key
   end if

   select case(toml_get_value_type(keyval%raw))
   case default
      call escape_string(keyval%raw, str)
      write(visitor%unit, '(a,a,a)', advance='no') &
         &  '{"type": "invalid", "value": "', str, '"}'

   case(toml_type%string)
      stat = convert_raw(keyval%raw, key)
      call escape_string(key, str)
      write(visitor%unit, '(a,a,a)', advance='no') &
         &  '{"type": "string", "value": "', str, '"}'

   case(toml_type%boolean)
      stat = convert_raw(keyval%raw, ldummy)
      if (ldummy) then
         write(visitor%unit, '(a)', advance='no') &
            &  '{"type": "bool", "value": "true"}'
      else
         write(visitor%unit, '(a)', advance='no') &
            &  '{"type": "bool", "value": "false"}'
      end if

   case(toml_type%int)
      stat = convert_raw(keyval%raw, idummy)
      write(visitor%unit, '(a,i0,a)', advance='no') &
         &  '{"type": "integer", "value": "', idummy, '"}'

   case(toml_type%float)
      stat = convert_raw(keyval%raw, fdummy)
      write(visitor%unit, '(a,g0,a)', advance='no') &
         &  '{"type": "float", "value": "', fdummy, '"}'

   case(toml_type%datetime)
      stat = convert_raw(keyval%raw, ts)
      write(visitor%unit, '(a)', advance='no') '{"type": "'
      if (allocated(ts%date)) write(visitor%unit, '(a)', advance='no') 'date'
      if (allocated(ts%time)) write(visitor%unit, '(a)', advance='no') 'time'
      call ts%to_string(str)
      write(visitor%unit, '(a,a,a)', advance='no') &
         &  '", "value": "', str, '"}'

   end select

end subroutine visit_keyval


!> Visit a TOML array
subroutine visit_array(visitor, array)

   !> Instance of the JSON serializer
   class(json_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_array), intent(inout) :: array

   class(toml_value), pointer :: ptr
   character(kind=tfc, len=:), allocatable :: key
   integer :: i, n
   logical :: array_of_tables

   call indent(visitor)

   if (allocated(array%key)) then
      call escape_string(array%key, key)
      write(visitor%unit, '("""",a,""": ")', advance='no') key
   end if

   array_of_tables = is_array_of_tables(array)
   if (.not.array_of_tables) then
      write(visitor%unit, '(a,a)', advance='no') &
         &  '{"type": "array", "value":'
      visitor%depth = visitor%depth + 1
      call indent(visitor)
   end if

   write(visitor%unit, '("[")', advance='no')
   visitor%depth = visitor%depth + 1
   n = len(array)
   do i = 1, n
      call array%get(i, ptr)
      call ptr%accept(visitor)
      if (i /= n) write(visitor%unit, '(",")', advance='no')
   end do
   visitor%depth = visitor%depth - 1
   call indent(visitor)
   write(visitor%unit, '("]")', advance='no')
   if (.not.array_of_tables) then
      visitor%depth = visitor%depth - 1
      call indent(visitor)
      write(visitor%unit, '("}")', advance='no')
   end if

end subroutine visit_array


!> Visit a TOML table
subroutine visit_table(visitor, table)

   !> Instance of the JSON serializer
   class(json_serializer), intent(inout) :: visitor

   !> TOML table to visit
   type(toml_table), intent(inout) :: table

   class(toml_value), pointer :: ptr
   type(toml_key), allocatable :: list(:)
   character(kind=tfc, len=:), allocatable :: key
   integer :: i, n

   call indent(visitor)

   if (allocated(table%key)) then
      call escape_string(table%key, key)
      write(visitor%unit, '("""",a,""": ")', advance='no') key
   end if

   write(visitor%unit, '("{")', advance='no')
   visitor%depth = visitor%depth + 1

   call table%get_keys(list)

   n = size(list, 1)
   do i = 1, n
      call table%get(list(i)%key, ptr)
      call ptr%accept(visitor)
      if (i /= n) write(visitor%unit, '(",")', advance='no')
   end do

   visitor%depth = visitor%depth - 1
   call indent(visitor)
   if (visitor%depth == 0) then
      if (allocated(visitor%indentation)) write(visitor%unit, '(a)')
      write(visitor%unit, '("}")')
   else
      write(visitor%unit, '("}")', advance='no')
   endif

end subroutine visit_table


!> Produce indentations for emitted JSON documents
subroutine indent(self)

   !> Instance of the JSON serializer
   class(json_serializer), intent(inout) :: self

   integer :: i

   ! PGI internal compiler error in NVHPC 20.7 and 20.9 with
   ! write(self%unit, '(/, a)', advance='no') repeat(self%indentation, self%depth)
   ! causes: NVFORTRAN-F-0000-Internal compiler error. Errors in Lowering      16
   if (allocated(self%indentation) .and. self%depth > 0) then
      write(self%unit, '(/)', advance='no')
      do i = 1, self%depth
         write(self%unit, '(a)', advance='no') self%indentation
      end do
   end if

end subroutine indent


!> Transform a TOML raw value to a JSON compatible escaped string
subroutine escape_string(raw, escaped)

   !> Raw value of TOML value
   character(len=*), intent(in) :: raw

   !> JSON compatible escaped string
   character(len=:), allocatable, intent(out) :: escaped

   integer :: i

   escaped = ''
   do i = 1, len(raw)
      select case(raw(i:i))
      case default; escaped = escaped // raw(i:i)
      case('\'); escaped = escaped // '\\'
      case('"'); escaped = escaped // '\"'
      case(TOML_NEWLINE); escaped = escaped // '\n'
      case(TOML_FORMFEED); escaped = escaped // '\f'
      case(TOML_CARRIAGE_RETURN); escaped = escaped // '\r'
      case(TOML_TABULATOR); escaped = escaped // '\t'
      case(TOML_BACKSPACE); escaped = escaped // '\b'
      end select
   end do

end subroutine escape_string


end module tftest_json_ser
