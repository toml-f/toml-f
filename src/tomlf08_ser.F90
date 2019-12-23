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
   use tomlf08_type
   implicit none

contains

recursive subroutine print_table(unit, table, stack, advance)
   type :: key_t
      character(len=:), allocatable :: key
   end type
   integer, intent(in) :: unit
   type(toml_table_t), intent(in) :: table
   type(key_t), intent(inout), target, optional :: stack(:)
   type(key_t), pointer :: sptr(:)
   character(len=*), intent(in), optional :: advance
   character(len=:), allocatable :: advance0
   integer :: i, j

   if (present(stack)) then
      sptr => stack
   else
      allocate(sptr(10))
   end if
   if (present(advance)) then
      advance0 = advance
   else
      advance0 = 'yes'
   endif

   if (advance0 == 'no') then
      do i = 1, table%nkeyval
         write(unit, '(1x,a,1x,"=",1x,a)', advance='no') &
            &  table%keyval(i)%key, table%keyval(i)%val
         if (i /= table%nkeyval) write(unit, '(",")', advance='no')
      end do
   else
      do i = 1, table%nkeyval
         write(unit, '(a,1x,"=",1x,a)') table%keyval(i)%key, table%keyval(i)%val
      end do
      write(unit, '(a)')
   end if

   do i = 1, table%narray
      select type(elem => table%array(i)%elem(:table%array(i)%nelem))
      type is(toml_table_t)
         call print_array_of_tables(unit, elem, table%array(i)%key, sptr) 
      class default
         write(unit, '(a,1x,"=",1x,"[")',advance='no') table%array(i)%key
         call print_array(unit, table%array(i), sptr)
         write(unit, '(1x,"]")')
      end select
   end do

   do i = 1, table%ntable
      write(unit, '("[")', advance='no')
      do j = 1, size(sptr, 1)
         if (allocated(sptr(j)%key)) then
            write(unit, '(a,".")', advance='no') sptr(j)%key
         else
            sptr(j)%key = table%table(i)%key
            write(unit, '(a,"]")') sptr(j)%key
            exit
         end if
      end do
      call print_table(unit, table%table(i), sptr)
      do j = size(sptr, 1), 1, -1
         if (allocated(sptr(j)%key)) then
            deallocate(sptr(j)%key)
            exit
         end if
      end do
   end do

   if (.not.present(stack)) deallocate(sptr)

contains

recursive subroutine print_array_of_tables(unit, tables, key, stack)
   integer, intent(in) :: unit
   type(toml_table_t), intent(in) :: tables(:)
   character(len=*), intent(in) :: key
   type(key_t), intent(inout) :: stack(:)
   integer :: i, j
   do i = 1, size(tables, 1)
      write(unit, '("[[")', advance='no')
      do j = 1, size(stack, 1)
         if (allocated(stack(j)%key)) then
            write(unit, '(a,".")', advance='no') stack(j)%key
         else
            sptr(j)%key = key
            write(unit, '(a,"]]")') key
            exit
         end if
      end do
      call print_table(unit, tables(i), stack)
      do j = size(sptr, 1), 1, -1
         if (allocated(sptr(j)%key)) then
            deallocate(sptr(j)%key)
            exit
         end if
      end do
   end do
end subroutine print_array_of_tables

recursive subroutine print_array(unit, array, stack)
   integer, intent(in) :: unit
   type(toml_array_t), intent(in) :: array
   type(key_t), intent(inout) :: stack(:)
   integer :: i
   select type(elem => array%elem)
   type is(toml_keyval_t)
      do i = 1, array%nelem
         write(unit, '(1x,a)', advance='no') elem(i)%val
         if (i /= array%nelem) write(unit, '(",")', advance='no')
      end do
   type is(toml_array_t)
      do i = 1, array%nelem
         write(unit, '(1x,"[")', advance='no')
         call print_array(unit, elem(i), stack)
         write(unit, '(1x,"]")', advance='no')
         if (i /= array%nelem) write(unit, '(",")', advance='no')
      end do
   type is(toml_table_t)
      do i = 1, array%nelem
         write(unit, '(1x,"{")', advance='no')
         call print_table(unit, elem(i), stack, advance='no')
         write(unit, '(1x,"}")', advance='no')
         if (i /= array%nelem) write(unit, '(",")', advance='no')
      end do
   end select
end subroutine print_array

end subroutine print_table

end module tomlf08_ser
