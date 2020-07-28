! This file is part of toml-f.
!
! Copyright (C) 2019-2020 Sebastian Ehlert
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

!> Functions to build a TOML datastructures
module tomlf_build
   use tomlf_constants, only : tfc
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, len
   implicit none
   private

   public :: add_table, add_array, add_keyval


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

   class(toml_value), allocatable :: new
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_table_(new)
   new%key = key
   call table%push_back(new, istat)

   if (allocated(new)) then
      call new%destroy
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

   class(toml_value), allocatable :: new
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_array_(new)
   new%key = key
   call table%push_back(new, istat)

   if (allocated(new)) then
      call new%destroy
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

   class(toml_value), allocatable :: new
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_keyval_(new)
   new%key = key
   call table%push_back(new, istat)

   if (allocated(new)) then
      call new%destroy
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

   class(toml_value), allocatable :: new
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_table_(new)
   call array%push_back(new, istat)

   if (allocated(new)) then
      call new%destroy
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

   class(toml_value), allocatable :: new
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   allocate(toml_array :: new)
   call new_array_(new)
   call array%push_back(new, istat)

   if (allocated(new)) then
      call new%destroy
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

   class(toml_value), allocatable :: new
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_keyval_(new)
   call array%push_back(new, istat)

   if (allocated(new)) then
      call new%destroy
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


subroutine new_table_(self)

   !> Newly created TOML table
   class(toml_value), allocatable, intent(out) :: self

   type(toml_table), allocatable :: new

   allocate(new)
   call new_table(new)
   call move_alloc(new, self)

end subroutine new_table_


subroutine new_array_(self)

   !> Newly created TOML array
   class(toml_value), allocatable, intent(out) :: self

   type(toml_array), allocatable :: new

   allocate(new)
   call new_array(new)
   call move_alloc(new, self)

end subroutine new_array_


subroutine new_keyval_(self)

   !> Newly created key-value pair
   class(toml_value), allocatable, intent(out) :: self

   type(toml_keyval), allocatable :: new

   allocate(new)
   call new_keyval(new)
   call move_alloc(new, self)

end subroutine new_keyval_


end module tomlf_build
