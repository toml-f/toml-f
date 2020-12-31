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

!> Collection of the central datatypes to define TOML data structures
!>
!> All TOML data types should inherit from an abstract value allowing to generate
!> a generic interface to deal with all more specialized TOML data types, while
!> the abstract value is interesting for developing algorithms in TOML-Fortran,
!> the user of TOML-Fortran will usually only care about TOML tables and possibly
!> arrays.
!>
!> The TOML types defined here should implement the TOML data structures (mostly)
!> without taking the actual implementation of the data structures into account.
!> This is done by providing a bare minimum interface using type bound procedures
!> to minimize the interdependencies between the datatypes.
!>
!> To make the data types extendable a visitor pattern allows access to the TOML
!> data types and can be used to implement further algorithms.
module tomlf_type
   use tomlf_constants, only : tfc
   use tomlf_error, only : toml_stat
   use tomlf_type_array, only : toml_array, new_array, new, len
   use tomlf_type_keyval, only : toml_keyval, new_keyval, new
   use tomlf_type_table, only : toml_table, new_table, new
   use tomlf_type_value, only : toml_value, toml_visitor, toml_key
   implicit none
   private

   public :: toml_value, toml_visitor, toml_table, toml_array, toml_keyval
   public :: toml_key
   public :: new, new_table, new_array, new_keyval, len
   public :: add_table, add_array, add_keyval
   public :: is_array_of_tables


   !> Interface to build new tables
   interface add_table
      module procedure :: add_table_to_table
      module procedure :: add_table_to_array
   end interface add_table


   !> Interface to build new arrays
   interface add_array
      module procedure :: add_array_to_table
      module procedure :: add_array_to_array
   end interface add_array


   !> Interface to build new key-value pairs
   interface add_keyval
      module procedure :: add_keyval_to_table
      module procedure :: add_keyval_to_array
   end interface add_keyval


contains


!> Create a new TOML table inside an existing table
subroutine add_table_to_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_table_(val)
   val%key = key
   call table%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call table%get(key, tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_table)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_table_to_table


!> Create a new TOML array inside an existing table
subroutine add_array_to_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new array
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created array
   type(toml_array), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_array_(val)
   val%key = key
   call table%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call table%get(key, tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_array)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_array_to_table


!> Create a new key-value pair inside an existing table
subroutine add_keyval_to_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new key-value pair
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created key-value pair
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_keyval_(val)
   val%key = key
   call table%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call table%get(key, tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_keyval)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_keyval_to_table


!> Create a new TOML table inside an existing array
subroutine add_table_to_array(array, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_table_(val)
   call array%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call array%get(len(array), tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_table)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_table_to_array


!> Create a new TOML array inside an existing array
subroutine add_array_to_array(array, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Pointer to the newly created array
   type(toml_array), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   allocate(toml_array :: val)
   call new_array_(val)
   call array%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call array%get(len(array), tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_array)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_array_to_array


!> Create a new key-value pair inside an existing array
subroutine add_keyval_to_array(array, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Pointer to the newly created key-value pair
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_keyval_(val)
   call array%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call array%get(len(array), tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_keyval)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_keyval_to_array


!> Wrapped constructor to create a new TOML table on an abstract TOML value
subroutine new_table_(self)

   !> Newly created TOML table
   class(toml_value), allocatable, intent(out) :: self

   type(toml_table), allocatable :: val

   allocate(val)
   call new_table(val)
   call move_alloc(val, self)

end subroutine new_table_


!> Wrapped constructor to create a new TOML array on an abstract TOML value
subroutine new_array_(self)

   !> Newly created TOML array
   class(toml_value), allocatable, intent(out) :: self

   type(toml_array), allocatable :: val

   allocate(val)
   call new_array(val)
   call move_alloc(val, self)

end subroutine new_array_


!> Wrapped constructor to create a new TOML array on an abstract TOML value
subroutine new_keyval_(self)

   !> Newly created key-value pair
   class(toml_value), allocatable, intent(out) :: self

   type(toml_keyval), allocatable :: val

   allocate(val)
   call new_keyval(val)
   call move_alloc(val, self)

end subroutine new_keyval_


!> Determine if array contains only tables
function is_array_of_tables(array) result(only_tables)

   !> TOML value to visit
   class(toml_array), intent(inout) :: array

   !> Array contains only tables
   logical :: only_tables

   class(toml_value), pointer :: ptr
   integer :: i, n


   n = len(array)
   only_tables = n > 0

   do i = 1, n
      call array%get(i, ptr)
      select type(ptr)
      type is(toml_table)
         cycle
      class default
         only_tables = .false.
         exit
      end select
   end do

end function is_array_of_tables


end module tomlf_type
