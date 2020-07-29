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

!> Functions to build a TOML data structures
!
!  The build module defines a high level interface to work with TOML data structures
!  and construct them in a convenient way.
module tomlf_build
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, len
   use tomlf_utils, only : toml_raw_to_string, toml_raw_to_float, &
      & toml_raw_to_bool, toml_raw_to_integer, toml_raw_to_timestamp
   implicit none
   private

   public :: add_table, add_array, add_keyval
   public :: get_value, set_value


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


   !> Setter functions to manipulate TOML tables
   interface set_value
      module procedure :: set_value_float_sp
      module procedure :: set_value_float_dp
      module procedure :: set_value_integer_i1
      module procedure :: set_value_integer_i2
      module procedure :: set_value_integer_i4
      module procedure :: set_value_integer_i8
      module procedure :: set_value_bool
      module procedure :: set_value_string
      module procedure :: set_child_value_float_sp
      module procedure :: set_child_value_float_dp
      module procedure :: set_child_value_integer_i1
      module procedure :: set_child_value_integer_i2
      module procedure :: set_child_value_integer_i4
      module procedure :: set_child_value_integer_i8
      module procedure :: set_child_value_bool
      module procedure :: set_child_value_string
   end interface set_value


   !> Getter functions to manipulate TOML tables
   interface get_value
      module procedure :: get_value_float_sp
      module procedure :: get_value_float_dp
      module procedure :: get_value_integer_i1
      module procedure :: get_value_integer_i2
      module procedure :: get_value_integer_i4
      module procedure :: get_value_integer_i8
      module procedure :: get_value_bool
      module procedure :: get_value_string
      module procedure :: get_child_table
      module procedure :: get_child_array
      module procedure :: get_child_keyval
      module procedure :: get_child_value_float_sp
      module procedure :: get_child_value_float_dp
      module procedure :: get_child_value_integer_i1
      module procedure :: get_child_value_integer_i2
      module procedure :: get_child_value_integer_i4
      module procedure :: get_child_value_integer_i8
      module procedure :: get_child_value_bool
      module procedure :: get_child_value_string
   end interface get_value


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


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_value_float_sp(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   real(tfr) :: dummy

   istat = toml_raw_to_float(self%raw, dummy)
   if (istat) then
      val = real(dummy, tf_sp)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_value_float_dp(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   real(tfr) :: dummy

   istat = toml_raw_to_float(self%raw, dummy)
   if (istat) then
      val = real(dummy, tf_dp)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_value_integer_i1(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   integer(tfi) :: dummy

   istat = toml_raw_to_integer(self%raw, dummy)
   if (istat) then
      val = int(dummy, tf_i1)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_value_integer_i2(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   integer(tfi) :: dummy

   istat = toml_raw_to_integer(self%raw, dummy)
   if (istat) then
      val = int(dummy, tf_i2)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_value_integer_i4(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   integer(tfi) :: dummy

   istat = toml_raw_to_integer(self%raw, dummy)
   if (istat) then
      val = int(dummy, tf_i4)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_value_integer_i8(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   integer(tfi) :: dummy

   istat = toml_raw_to_integer(self%raw, dummy)
   if (istat) then
      val = int(dummy, tf_i8)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_value_bool(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Boolean value
   logical, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat

   istat = toml_raw_to_bool(self%raw, val)
   if (istat) then
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_bool


!> Retrieve TOML value as deferred-length character
subroutine get_value_string(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat

   istat = toml_raw_to_string(self%raw, val)
   if (istat) then
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_string


subroutine get_child_table(table, key, ptr, requested, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child table
   class(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp
   logical :: is_requested

   if (present(requested)) then
      is_requested = requested
   else
      is_requested = .true.
   end if

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_table)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      if (is_requested) then
         call add_table(table, key, ptr, stat)
      end if
   end if

end subroutine get_child_table


subroutine get_child_array(table, key, ptr, requested, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child array
   class(toml_array), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp
   logical :: is_requested

   if (present(requested)) then
      is_requested = requested
   else
      is_requested = .true.
   end if

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_array)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      call add_array(table, key, ptr, stat)
   end if

end subroutine get_child_array


subroutine get_child_keyval(table, key, ptr, requested, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child value
   class(toml_keyval), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp
   logical :: is_requested

   if (present(requested)) then
      is_requested = requested
   else
      is_requested = .true.
   end if

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_keyval)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      call add_keyval(table, key, ptr, stat)
   end if

end subroutine get_child_keyval


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_child_value_float_sp(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Default real value
   real(tf_sp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   end if

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            !call set_value(ptr, default)
            val = default
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_child_value_float_dp(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Default real value
   real(tf_dp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   end if

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            !call set_value(ptr, default)
            val = default
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_child_value_integer_i1(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Default integer value
   integer(tf_i1), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   end if

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            !call set_value(ptr, default)
            val = default
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_child_value_integer_i2(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Default integer value
   integer(tf_i2), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   end if

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            !call set_value(ptr, default)
            val = default
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_child_value_integer_i4(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Default integer value
   integer(tf_i4), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   end if

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            !call set_value(ptr, default)
            val = default
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_child_value_integer_i8(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Default integer value
   integer(tf_i8), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            !call set_value(ptr, default)
            val = default
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_child_value_bool(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Boolean value
   logical, intent(out) :: val

   !> Default boolean value
   logical, intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            !call set_value(ptr, default)
            val = default
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_bool


!> Retrieve TOML value as deferred-length character
subroutine get_child_value_string(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Default string value
   character(kind=tfc, len=*), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            !call set_value(ptr, default)
            val = default
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_string


!> Set TOML value to single precision float
subroutine set_value_float_sp(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=128), allocatable :: tmp
   integer :: istat

   write(tmp, '(es30.6)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(tmp)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_float_sp


!> Set TOML value to double precision float
subroutine set_value_float_dp(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=128), allocatable :: tmp
   integer :: istat

   write(tmp, '(es30.16)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(tmp)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_float_dp


!> Set TOML value to one byte integer
subroutine set_value_integer_i1(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=128), allocatable :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(tmp)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_value_integer_i2(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=128), allocatable :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(tmp)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_value_integer_i4(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=128), allocatable :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(tmp)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_value_integer_i8(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=128), allocatable :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(tmp)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_integer_i8


!> Set TOML value to logical
subroutine set_value_bool(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   if (val) then
      self%raw = 'true'
   else
      self%raw = 'false'
   end if

   if (present(stat)) stat = toml_stat%success

end subroutine set_value_bool


!> Set TOML value to deferred-length character
subroutine set_value_string(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   self%raw = val

   if (present(stat)) stat = toml_stat%success

end subroutine set_value_string


!> Set TOML value to single precision float
subroutine set_child_value_float_sp(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_float_sp


!> Set TOML value to double precision float
subroutine set_child_value_float_dp(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_float_dp


!> Set TOML value to one byte integer
subroutine set_child_value_integer_i1(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_child_value_integer_i2(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_child_value_integer_i4(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_child_value_integer_i8(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i8


!> Set TOML value to logical
subroutine set_child_value_bool(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_bool


!> Set TOML value to deferred-length character
subroutine set_child_value_string(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_string


end module tomlf_build
