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

!> TOML serializer implementation
module tomlf_ser
   use tomlf_constants, only : tfc, tfout
   use tomlf_type, only : toml_value, toml_visitor, toml_key, toml_table, &
      & toml_array, toml_keyval, is_array_of_tables, len
   implicit none
   private

   public :: toml_serializer, new_serializer, new


   !> TOML serializer to produduce a TOML document from a datastructure
   type, extends(toml_visitor) :: toml_serializer

      !> Unit for output
      integer :: unit = tfout

      !> Special mode for printing array of tables
      logical, private :: array_of_tables = .false.

      !> Special mode for printing inline arrays
      logical, private :: inline_array = .false.

      !> Top of the key stack
      integer, private :: top = 0

      !> Key stack to create table headers
      type(toml_key), allocatable, private :: stack(:)

   contains

      !> Visit a TOML value
      procedure :: visit

   end type toml_serializer


   !> Create standard constructor
   interface toml_serializer
      module procedure :: new_serializer_func
   end interface toml_serializer


   !> Overloaded constructor for TOML serializers
   interface new
      module procedure :: new_serializer
   end interface


   !> Initial size of the key path stack
   integer, parameter :: initial_size = 8


contains


!> Constructor to create new serializer instance
subroutine new_serializer(self, unit)

   !> Instance of the TOML serializer
   type(toml_serializer), intent(out) :: self

   !> Unit for IO
   integer, intent(in), optional :: unit

   if (present(unit)) then
      self%unit = unit
   end if

end subroutine new_serializer


!> Default constructor for TOML serializer
function new_serializer_func(unit) result(self)

   !> Unit for IO
   integer, intent(in), optional :: unit

   !> Instance of the TOML serializer
   type(toml_serializer) :: self

   call new_serializer(self, unit)

end function new_serializer_func


!> Visit a TOML value
recursive subroutine visit(self, val)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: self

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

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_keyval), intent(inout) :: keyval

   character(kind=tfc, len=:), allocatable :: key

   call keyval%get_key(key)

   if (visitor%inline_array) then
      write(visitor%unit, '(1x,a,1x,"=",1x,a,",")', advance='no') &
         &  key, keyval%raw
   else
      write(visitor%unit, '(a,1x,"=",1x,a)') key, keyval%raw
   end if

end subroutine visit_keyval


!> Visit a TOML array
recursive subroutine visit_array(visitor, array)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_array), intent(inout) :: array

   class(toml_value), pointer :: ptr
   character(kind=tfc, len=:), allocatable :: key
   integer :: i, n

   if (visitor%inline_array) write(visitor%unit, '(1x,"[")', advance='no')
   n = len(array)
   do i = 1, n
      call array%get(i, ptr)
      select type(ptr)
      class is(toml_keyval)
         write(visitor%unit, '(1x,a)', advance='no') ptr%raw
         if (i /= n) write(visitor%unit, '(",")', advance='no')
      class is(toml_array)
         call ptr%accept(visitor)
         if (i /= n) write(visitor%unit, '(",")', advance='no')
      class is(toml_table)
         if (visitor%inline_array) then
            write(visitor%unit, '(1x,"{")', advance='no')
            call ptr%accept(visitor)
            write(visitor%unit, '(1x,"}")', advance='no')
            if (i /= n) write(visitor%unit, '(",")', advance='no')
         else
            visitor%array_of_tables = .true.
            if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
            visitor%top = visitor%top + 1
            call array%get_key(key)
            visitor%stack(visitor%top)%key = key
            call ptr%accept(visitor)
            deallocate(visitor%stack(visitor%top)%key)
            visitor%top = visitor%top - 1
         end if
      end select
   end do
   if (visitor%inline_array) write(visitor%unit, '(1x,"]")', advance='no')

end subroutine visit_array


!> Visit a TOML table
recursive subroutine visit_table(visitor, table)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML table to visit
   type(toml_table), intent(inout) :: table

   class(toml_value), pointer :: ptr
   type(toml_key), allocatable :: list(:)
   logical, allocatable :: defer(:)
   character(kind=tfc, len=:), allocatable :: key
   integer :: i, n

   call table%get_keys(list)

   n = size(list, 1)
   allocate(defer(n))

   if (.not.allocated(visitor%stack)) then
      call resize(visitor%stack)
   else
      if (.not.(visitor%inline_array .or. table%implicit)) then
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

   do i = 1, n
      defer(i) = .false.
      call table%get(list(i)%key, ptr)
      select type(ptr)
      class is(toml_keyval)
         call ptr%accept(visitor)
      class is(toml_array)
         if (visitor%inline_array) then
            call ptr%get_key(key)
            write(visitor%unit, '(1x,a,1x,"=")', advance='no') key
            call ptr%accept(visitor)
            if (i /= n) write(visitor%unit, '(",")', advance='no')
         else
            if (is_array_of_tables(ptr)) then
               ! Array of tables open a new section
               ! -> cannot serialize them before all key-value pairs are done
               defer(i) = .true.
            else
               visitor%inline_array = .true.
               call ptr%get_key(key)
               write(visitor%unit, '(a,1x,"=")', advance='no') key
               call ptr%accept(visitor)
               visitor%inline_array = .false.
               write(visitor%unit, '(a)')
            end if
         end if
      class is(toml_table)
         ! Subtables open a new section
         ! -> cannot serialize them before all key-value pairs are done
         defer(i) = .true.
      end select
   end do

   do i = 1, n
      if (defer(i)) then
         call table%get(list(i)%key, ptr)
         select type(ptr)
         class is(toml_keyval)
            call ptr%accept(visitor)
         class is(toml_array)
            if (visitor%inline_array) then
               call ptr%get_key(key)
               write(visitor%unit, '(1x,a,1x,"=")', advance='no') key
               call ptr%accept(visitor)
               if (i /= n) write(visitor%unit, '(",")', advance='no')
            else
               if (is_array_of_tables(ptr)) then
                  call ptr%accept(visitor)
               else
                  visitor%inline_array = .true.
                  call ptr%get_key(key)
                  write(visitor%unit, '(a,1x,"=")', advance='no') key
                  call ptr%accept(visitor)
                  visitor%inline_array = .false.
                  write(visitor%unit, '(a)')
               end if
            end if
         class is(toml_table)
            if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
            visitor%top = visitor%top + 1
            call ptr%get_key(key)
            visitor%stack(visitor%top)%key = key
            call ptr%accept(visitor)
            deallocate(visitor%stack(visitor%top)%key)
            visitor%top = visitor%top - 1
         end select
      end if
   end do

   if (.not.visitor%inline_array .and. visitor%top == 0) then
      deallocate(visitor%stack)
   end if

end subroutine visit_table


!> Change size of the stack
subroutine resize(stack, n)

   !> Stack of keys to be resized
   type(toml_key), allocatable, intent(inout) :: stack(:)

   !> New size of the stack
   integer, intent(in), optional :: n

   type(toml_key), allocatable :: tmp(:)
   integer :: m

   if (present(n)) then
      m = n
   else
      if (allocated(stack)) then
         m = size(stack)
         m = m + m/2 + 1
      else
         m = initial_size
      end if
   end if

   if (allocated(stack)) then
      call move_alloc(stack, tmp)
      allocate(stack(m))

      m = min(size(tmp), m)
      stack(:m) = tmp(:m)

      deallocate(tmp)
   else
      allocate(stack(m))
   end if

end subroutine resize


end module tomlf_ser
