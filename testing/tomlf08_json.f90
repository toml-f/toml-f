! This file is part of toml-f.
!
! Copyright (C) 2019-2020 Sebastian Ehlert
!
! toml-f is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! toml-f is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with toml-f.  If not, see <https://www.gnu.org/licenses/>.

!> Serialization of datastructures to json for testing purposes.
module tomlf08_json
   use iso_fortran_env, only: output_unit, error_unit
   use tomlf08_type
   implicit none
   private
   public :: json_serializer

   !> Serializer to produduce a JSON file from a datastructure.
   type, extends(toml_visitor) :: json_serializer
      !> Unit for output.
      integer, public :: unit = output_unit
      !> Close the output unit in deconstructor.
      logical :: close_unit = .false.
   contains
      !> Hook to visit a table node.
      procedure :: visit_table => ser_visit_table
      !> Hook to visit an array node.
      procedure :: visit_array => ser_visit_array
      !> Hook to visit a key-value pair.
      procedure :: visit_keyval => ser_visit_keyval
      !> Deconstructor for the serializer.
      procedure :: destroy => ser_destroy
      !> Automatic deconstructor for the serializer to close still open units.
      final :: ser_final
   end type json_serializer

   !> Constructors for the JSON serializer.
   interface json_serializer
      !> Construct a serializer from a IO unit.
      module procedure :: ser_new_from_unit
      !> Construct a serializer from a file name and create unit on-the-fly.
      module procedure :: ser_new_from_file
   end interface json_serializer

contains

!> Construct a TOML serializer from a IO unit.
type(json_serializer) function ser_new_from_unit(unit) result(self)
   integer, intent(in) :: unit
   self%unit = unit
end function ser_new_from_unit

!> Construct a TOML serializer from a file name and create unit on-the-fly.
type(json_serializer) function ser_new_from_file(file) result(self)
   character(len=*), intent(in) :: file
   integer :: unit
   open(file=file, newunit=unit)
   self%unit = unit
   self%close_unit = .true.
end function ser_new_from_file

!> Deconstructor for the serializer to close still open units.
!  Stack should be already deallocated and all units at default values.
subroutine ser_destroy(self)
   !> Serializer instance.
   class(json_serializer), intent(inout) :: self
   logical :: opened
   if (self%unit /= output_unit .and. self%unit /= error_unit) then
      inquire(unit=self%unit, opened=opened)
      if (opened .and. self%close_unit) then
         close(self%unit)
      end if
   end if
   self%close_unit = .false.
end subroutine ser_destroy

!> Automatic deconstructor for the serializer.
subroutine ser_final(self)
   !> Serializer instance.
   type(json_serializer), intent(inout) :: self
   call self%destroy
end subroutine ser_final

!> Serializer transversing a table, usual entry point for serialization.
recursive subroutine ser_visit_table(visitor, table)
   !> Serializer instance.
   class(json_serializer), intent(inout) :: visitor
   !> TOML table.
   class(toml_table), intent(in) :: table
   integer :: i
   if (allocated(table%key)) then
      write(visitor%unit, '("""",a,""":")', advance='no') table%key
   end if
   write(visitor%unit, '("{")', advance='no')
   do i = 1, table%nkeyval
      call visitor%visit(table%keyval(i))
      if (i /= table%nkeyval .or. table%narray > 0) &
         & write(visitor%unit, '(",")', advance='no')
   end do
   do i = 1, table%narray
      call visitor%visit(table%array(i))
      if (i /= table%narray .or. table%ntable > 0) &
         & write(visitor%unit, '(",")', advance='no')
   end do
   do i = 1, table%ntable
      call visitor%visit(table%table(i))
      if (i /= table%ntable) write(visitor%unit, '(",")', advance='no')
   end do
   write(visitor%unit, '("}")', advance='no')
end subroutine ser_visit_table

!> Serializer transversing an array.
recursive subroutine ser_visit_array(visitor, array)
   !> Serializer instance.
   class(json_serializer), intent(inout) :: visitor
   !> TOML array.
   class(toml_array), intent(in) :: array
   integer :: i
   if (allocated(array%key)) then
      write(visitor%unit, '("""",a,""":")', advance='no') array%key
   end if
   write(visitor%unit, '("[")', advance='no')
   do i = 1, array%nelem
      call array%elem(i)%accept(visitor)
   end do
   write(visitor%unit, '("]")', advance='no')
end subroutine ser_visit_array

!> Serializer visiting a key-value pair.
subroutine ser_visit_keyval(visitor, keyval)
   use tomlf08_utils
   !> Serializer instance.
   class(json_serializer), intent(inout) :: visitor
   !> TOML Key-value pair.
   class(toml_keyval), intent(in) :: keyval
   if (allocated(keyval%key)) then
      write(visitor%unit, '("""",a,""":")', advance='no') keyval%key
   end if
   select case(toml_get_value_type(keyval%val))
   case(STRING_TYPE)
      write(visitor%unit, '(a,1x,a,1x,a)', advance='no') &
         &  '{"type": "string", "value": "', keyval%val, '"}'
   case(BOOL_TYPE)
      write(visitor%unit, '(a,1x,a,1x,a)', advance='no') &
         &  '{"type": "bool", "value": "', keyval%val, '"}'
   case(INTEGER_TYPE)
      write(visitor%unit, '(a,1x,i0,1x,a)', advance='no') &
         &  '{"type": "integer", "value": "', keyval%val, '"}'
   case(FLOAT_TYPE)
      write(visitor%unit, '(a,1x,g0,1x,a)', advance='no') &
         &  '{"type": "float", "value": "', keyval%val, '"}'
   case(TIMESTAMP_TYPE)
      write(visitor%unit, '(a,1x,a,1x,a)', advance='no') &
         &  '{"type": "datetime", "value": "', keyval%val, '"}'
   end select
end subroutine ser_visit_keyval

end module tomlf08_json

program toml2json
   use iso_fortran_env
   use tomlf08
   use tomlf08_json
   implicit none
   integer :: iarg, length
   character(len=:), allocatable :: argument
   type(toml_table), allocatable :: table
   type(json_serializer) :: ser
   integer :: unit
   logical :: exist

   if (command_argument_count() > 0) then
      do iarg = 1, command_argument_count()
         if (allocated(argument)) deallocate(argument)
         call get_command_argument(iarg, length=length)
         allocate(character(len=length) :: argument)
         call get_command_argument(iarg, argument)
         inquire(file=argument, exist=exist)
         if (exist) then
            open(newunit=unit, file=argument)
            if (allocated(table)) deallocate(table)
            call toml_parse(table, unit)
            close(unit)
            if (allocated(table)) then
               call table%accept(ser)
               call table%destroy
            end if
         end if
      end do
   else
      call toml_parse(table, input_unit)
      if (allocated(table)) then
         call table%accept(ser)
         call table%destroy
      end if
   end if
   if (allocated(argument)) deallocate(argument)
end program toml2json
