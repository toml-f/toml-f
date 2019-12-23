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
   use iso_fortran_env, only: output_unit
   use tomlf08_type
   implicit none
   private
   public :: toml_serializer_t

   type :: key_t
      character(len=:), allocatable :: key
   end type

   type, extends(toml_visitor_t) :: toml_serializer_t
      private
      integer, public :: unit = output_unit
      logical :: array_of_tables = .false.
      integer :: top = 0
      type(key_t), allocatable :: stack(:)
   contains
      procedure :: visit_table => ser_visit_table
      procedure :: visit_array => ser_visit_array
      procedure :: visit_keyval => ser_visit_keyval
      !procedure, private :: table_header => ser_table_header
   end type toml_serializer_t

   interface resize
      module procedure :: ser_resize_stack
   end interface

contains

subroutine ser_visit_table(visitor, table)
   class(toml_serializer_t), intent(inout) :: visitor
   class(toml_table_t), intent(inout) :: table
   integer :: i
   if (.not.allocated(visitor%stack)) then
      allocate(visitor%stack(10))
   else
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
   do i = 1, table%nkeyval
      call visitor%visit(table%keyval(i))
   end do
   if (visitor%top > 0) write(visitor%unit, '(a)')
   do i = 1, table%narray
      if (table%array(i)%get_kind() == TABLE_KIND) then
         call visitor%visit(table%array(i))
      else
         write(visitor%unit, '(a,1x,"=")',advance='no') table%array(i)%key
         write(visitor%unit, '(1x,"[")',advance='no')
         call visitor%visit(table%array(i))
         write(visitor%unit, '(1x,"]")')
      end if
   end do
   do i = 1, table%ntable
      if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
      visitor%top = visitor%top + 1
      visitor%stack(visitor%top)%key = table%table(i)%key
      call visitor%visit(table%table(i))
      deallocate(visitor%stack(visitor%top)%key)
      visitor%top = visitor%top - 1
   end do
   if (visitor%top == 0) then
      deallocate(visitor%stack)
   end if
end subroutine ser_visit_table

subroutine ser_visit_array(visitor, array)
   class(toml_serializer_t), intent(inout) :: visitor
   class(toml_array_t), intent(inout) :: array
   integer :: i, j
   select type(elem => array%elem)
   class is(toml_keyval_t)
      do i = 1, array%nelem
         write(visitor%unit, '(1x,a)', advance='no') elem(i)%val
         if (i /= array%nelem) write(visitor%unit, '(",")', advance='no')
      end do
   class is(toml_array_t)
      do i = 1, array%nelem
         write(visitor%unit, '(1x,"[")', advance='no')
         call visitor%visit(elem(i))
         write(visitor%unit, '(1x,"]")', advance='no')
         if (i /= array%nelem) write(visitor%unit, '(",")', advance='no')
      end do
   class is(toml_table_t)
      do i = 1, array%nelem
         visitor%array_of_tables = .true.
         if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
         visitor%top = visitor%top + 1
         visitor%stack(visitor%top)%key = array%key
         call visitor%visit(elem(i))
         deallocate(visitor%stack(visitor%top)%key)
         visitor%top = visitor%top - 1
      end do
   end select
end subroutine ser_visit_array

subroutine ser_visit_keyval(visitor, keyval)
   class(toml_serializer_t), intent(inout) :: visitor
   class(toml_keyval_t), intent(inout) :: keyval
   write(visitor%unit, '(a,1x,"=",1x,a)') keyval%key, keyval%val
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
