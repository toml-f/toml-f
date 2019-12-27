! This file is part of toml-f.
!
! Copyright (C) 2019 Sebastian Ehlert
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

!> Serialization module for TOML.
module tomlf08_ser
   use iso_fortran_env, only: output_unit, error_unit
   use tomlf08_type
   implicit none
   private
   public :: toml_serializer_t

   !> Wrapper for a deferred length character instances in an array.
   type :: key_t
      !> TOML key.
      character(len=:), allocatable :: key
   end type

   !> TOML serializer to produduce a TOML file from a datastructure.
   type, extends(toml_visitor_t) :: toml_serializer_t
      private
      !> Unit for output.
      integer, public :: unit = output_unit
      !> Special mode for printing arrays of tables.
      logical :: array_of_tables = .false.
      !> Special mode for printing arrays of tables.
      logical :: inline_array = .false.
      !> Top of the key stack.
      integer :: top = 0
      !> Key stack to create table headers.
      type(key_t), allocatable :: stack(:)
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
   end type toml_serializer_t

   !> Constructors for the TOML serializer.
   interface toml_serializer_t
      !> Construct a serializer from a IO unit.
      module procedure :: ser_new_from_unit
      !> Construct a serializer from a file name and create unit on-the-fly.
      module procedure :: ser_new_from_file
   end interface toml_serializer_t

   !> Dynamic array interface.
   interface resize
      !> Resize internal stack of the TOML serializer.
      module procedure :: ser_resize_stack
   end interface

contains

!> Construct a TOML serializer from a IO unit.
type(toml_serializer_t) function ser_new_from_unit(unit) result(self)
   integer, intent(in) :: unit
   self%unit = unit
end function ser_new_from_unit

!> Construct a TOML serializer from a file name and create unit on-the-fly.
type(toml_serializer_t) function ser_new_from_file(file) result(self)
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
   class(toml_serializer_t), intent(inout) :: self
   logical :: opened
   if (self%unit /= output_unit .and. self%unit /= error_unit) then
      inquire(unit=self%unit, opened=opened)
      if (opened .and. self%close_unit) then
         close(self%unit)
      end if
   end if
   self%array_of_tables = .false.
   self%close_unit = .false.
   self%top = 0
   if (allocated(self%stack)) deallocate(self%stack)
end subroutine ser_destroy

!> Automatic deconstructor for the serializer.
subroutine ser_final(self)
   !> Serializer instance.
   type(toml_serializer_t), intent(inout) :: self
   call self%destroy
end subroutine ser_final

!> Serializer transversing a table, usual entry point for serialization.
recursive subroutine ser_visit_table(visitor, table)
   !> Serializer instance.
   class(toml_serializer_t), intent(inout) :: visitor
   !> TOML table.
   class(toml_table_t), intent(inout) :: table
   integer :: i
   if (.not.allocated(visitor%stack)) then
      allocate(visitor%stack(10))
   else
      if (.not.visitor%inline_array) then
         write(visitor%unit, '("[")', advance='no')
         if (visitor%array_of_tables) write(visitor%unit, '("[")', advance='no')
         do i = 1, visitor%top-1
            write(visitor%unit, '(a,".")', advance='no') visitor%stack(i)%key
         end do
         write(visitor%unit, '(a)', advance='no') visitor%stack(visitor%top)%key
         write(visitor%unit, '("]")', advance='no')
         if (visitor%array_of_tables) write(visitor%unit, '("]")', advance='no')
         write(visitor%unit, '(a)')
         visitor%array_of_tables = .false.
      end if
   end if
   do i = 1, table%nkeyval
      call visitor%visit(table%keyval(i))
   end do
   do i = 1, table%narray
      if (visitor%inline_array) then
         write(visitor%unit, '(1x,a,1x,"=")', advance='no') table%array(i)%key
         call visitor%visit(table%array(i))
         write(visitor%unit, '(",")', advance='no')
      else
         if (table%array(i)%get_kind() == TABLE_KIND) then
            call visitor%visit(table%array(i))
         else
            visitor%inline_array = .true.
            write(visitor%unit, '(a,1x,"=")', advance='no') table%array(i)%key
            call visitor%visit(table%array(i))
            visitor%inline_array = .false.
            write(visitor%unit, '(a)')
         end if
      end if
   end do
   if (.not. visitor%inline_array .and. visitor%top > 0) write(visitor%unit, '(a)')
   do i = 1, table%ntable
      if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
      visitor%top = visitor%top + 1
      visitor%stack(visitor%top)%key = table%table(i)%key
      call visitor%visit(table%table(i))
      deallocate(visitor%stack(visitor%top)%key)
      visitor%top = visitor%top - 1
   end do
   if (.not.visitor%inline_array .and. visitor%top == 0) then
      deallocate(visitor%stack)
   end if
end subroutine ser_visit_table

!> Serializer transversing an array.
recursive subroutine ser_visit_array(visitor, array)
   !> Serializer instance.
   class(toml_serializer_t), intent(inout) :: visitor
   !> TOML array.
   class(toml_array_t), intent(inout) :: array
   integer :: i, j
   if (visitor%inline_array) write(visitor%unit, '(1x,"[")', advance='no')
   select type(elem => array%elem)
   class is(toml_keyval_t)
      do i = 1, array%nelem
         write(visitor%unit, '(1x,a)', advance='no') elem(i)%val
         if (i /= array%nelem) write(visitor%unit, '(",")', advance='no')
      end do
   class is(toml_array_t)
      do i = 1, array%nelem
         call visitor%visit(elem(i))
         if (i /= array%nelem) write(visitor%unit, '(",")', advance='no')
      end do
   class is(toml_table_t)
      if (visitor%inline_array) then
         do i = 1, array%nelem
            write(visitor%unit, '(1x,"{")', advance='no')
            call visitor%visit(elem(i))
            write(visitor%unit, '(1x,"}")', advance='no')
            if (i /= array%nelem) write(visitor%unit, '(",")', advance='no')
         end do
      else
         do i = 1, array%nelem
            visitor%array_of_tables = .true.
            if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
            visitor%top = visitor%top + 1
            visitor%stack(visitor%top)%key = array%key
            call visitor%visit(elem(i))
            deallocate(visitor%stack(visitor%top)%key)
            visitor%top = visitor%top - 1
         end do
      end if
   end select
   if (visitor%inline_array) write(visitor%unit, '(1x,"]")', advance='no')
end subroutine ser_visit_array

!> Serializer visiting a key-value pair.
subroutine ser_visit_keyval(visitor, keyval)
   !> Serializer instance.
   class(toml_serializer_t), intent(inout) :: visitor
   !> TOML Key-value pair.
   class(toml_keyval_t), intent(inout) :: keyval
   if (visitor%inline_array) then
      write(visitor%unit, '(1x,a,1x,"=",1x,a,",")', advance='no') &
         &  keyval%key, keyval%val
   else
      write(visitor%unit, '(a,1x,"=",1x,a)') keyval%key, keyval%val
   end if
end subroutine ser_visit_keyval

subroutine ser_resize_stack(stack, n)
   type(key_t), allocatable, intent(inout) :: stack(:)
   integer, intent(in), optional :: n
   type(key_t), allocatable :: tmp(:)
   integer :: this_size, new_size
   if (allocated(stack)) then
      this_size = size(stack, 1)
      call move_alloc(stack, tmp)
   else
      this_size = 6
   end if

   if (present(n)) then
      new_size = n
   else
      new_size = this_size + this_size/2 + 1
   end if

   allocate(stack(new_size))

   if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(stack, 1))
      stack(:this_size) = tmp(:this_size)
      deallocate(tmp)
   end if
end subroutine ser_resize_stack

end module tomlf08_ser
