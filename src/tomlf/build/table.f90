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

!> Functions to build TOML tables
!>
!> The build module defines a high level interface to work with TOML tables
!> and construct them in a convenient way.
!>
!> The getter functions allow to both retrieve and set values, to easily
!> support default values when reading from a TOML data structure.
!> Using the getter function with a default value specified will request
!> the respective setter function to add it to the table if it was not
!> found in the first place.
!>
!> This allows to build a TOML table using only the getter functions, which
!> represents the finally read values for the applications.
!>
!> Note that neither setter nor getter functions can overwrite existing
!> TOML values for safety reasons, request the deletion on the respective
!> key from the TOML table and than set it. The deletion of a subtable or
!> array will recursively destroy the contained data nodes.
module tomlf_build_table
   use tomlf_build_keyval, only : get_value, set_value
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp
   use tomlf_datetime, only : toml_datetime
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, add_table, add_array, add_keyval, &
      & toml_key, cast_to_table, cast_to_array, cast_to_keyval, len
   implicit none
   private

   public :: get_value, set_value


   !> Setter functions to manipulate TOML tables
   interface set_value
      module procedure :: set_child_value_float_sp
      module procedure :: set_child_value_float_dp
      module procedure :: set_child_value_integer_i1
      module procedure :: set_child_value_integer_i2
      module procedure :: set_child_value_integer_i4
      module procedure :: set_child_value_integer_i8
      module procedure :: set_child_value_bool
      module procedure :: set_child_value_datetime
      module procedure :: set_child_value_string
      module procedure :: set_key_value_float_sp
      module procedure :: set_key_value_float_dp
      module procedure :: set_key_value_integer_i1
      module procedure :: set_key_value_integer_i2
      module procedure :: set_key_value_integer_i4
      module procedure :: set_key_value_integer_i8
      module procedure :: set_key_value_bool
      module procedure :: set_key_value_datetime
      module procedure :: set_key_value_string
   end interface set_value


   !> Getter functions to manipulate TOML tables
   interface get_value
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
      module procedure :: get_child_value_datetime
      module procedure :: get_child_value_string
      module procedure :: get_key_table
      module procedure :: get_key_array
      module procedure :: get_key_keyval
      module procedure :: get_key_value_float_sp
      module procedure :: get_key_value_float_dp
      module procedure :: get_key_value_integer_i1
      module procedure :: get_key_value_integer_i2
      module procedure :: get_key_value_integer_i4
      module procedure :: get_key_value_integer_i8
      module procedure :: get_key_value_bool
      module procedure :: get_key_value_datetime
      module procedure :: get_key_value_string
   end interface get_value


contains


subroutine get_key_table(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, ptr, requested, stat, origin)

end subroutine get_key_table


subroutine get_key_array(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, ptr, requested, stat, origin)

end subroutine get_key_array


subroutine get_key_keyval(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, ptr, requested, stat, origin)

end subroutine get_key_keyval


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_key_value_float_sp(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Default real value
   real(tf_sp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_key_value_float_dp(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Default real value
   real(tf_dp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_key_value_integer_i1(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Default integer value
   integer(tf_i1), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_key_value_integer_i2(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Default integer value
   integer(tf_i2), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_key_value_integer_i4(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Default integer value
   integer(tf_i4), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_key_value_integer_i8(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Default integer value
   integer(tf_i8), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_key_value_bool(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Boolean value
   logical, intent(out) :: val

   !> Default boolean value
   logical, intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_bool


!> Retrieve TOML value as datetime
subroutine get_key_value_datetime(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Datetime value
   type(toml_datetime), intent(out) :: val

   !> Default datetime value
   type(toml_datetime), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_datetime


!> Retrieve TOML value as deferred-length character
subroutine get_key_value_string(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Default string value
   character(kind=tfc, len=*), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call get_value(table, key%key, val, default, stat, origin)

end subroutine get_key_value_string


!> Set TOML value to single precision float
subroutine set_key_value_float_sp(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_float_sp


!> Set TOML value to double precision float
subroutine set_key_value_float_dp(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_float_dp


!> Set TOML value to one byte integer
subroutine set_key_value_integer_i1(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_key_value_integer_i2(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_key_value_integer_i4(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_key_value_integer_i8(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_integer_i8


!> Set TOML value to logical
subroutine set_key_value_bool(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_bool


!> Set TOML value to datetime
subroutine set_key_value_datetime(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Datetime value
   type(toml_datetime), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_datetime


!> Set TOML value to deferred-length character
subroutine set_key_value_string(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   call set_value(table, key%key, val, stat, origin)

end subroutine set_key_value_string


subroutine get_child_table(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

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
      ptr => cast_to_table(tmp)
      if (present(stat)) then
         if (associated(ptr)) then
            stat = toml_stat%success
         else
            stat = toml_stat%type_mismatch
         end if
      end if
      if (present(origin)) origin = tmp%origin
   else
      if (is_requested) then
         call add_table(table, key, ptr, stat)
      else
         if (present(stat)) stat = toml_stat%success
      end if
      if (present(origin)) origin = table%origin
   end if

end subroutine get_child_table


subroutine get_child_array(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

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
      ptr => cast_to_array(tmp)
      if (present(stat)) then
         if (associated(ptr)) then
            stat = toml_stat%success
         else
            stat = toml_stat%type_mismatch
         end if
      end if
      if (present(origin)) origin = tmp%origin
   else
      if (is_requested) then
         call add_array(table, key, ptr, stat)
      else
         if (present(stat)) stat = toml_stat%success
      end if
      if (present(origin)) origin = table%origin
   end if

end subroutine get_child_array


subroutine get_child_keyval(table, key, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

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
      ptr => cast_to_keyval(tmp)
      if (present(stat)) then
         if (associated(ptr)) then
            stat = toml_stat%success
         else
            stat = toml_stat%type_mismatch
         end if
      end if
      if (present(origin)) origin = tmp%origin
   else
      if (is_requested) then
         call add_keyval(table, key, ptr, stat)
      else
         if (present(stat)) stat = toml_stat%success
      end if
      if (present(origin)) origin = table%origin
   end if

end subroutine get_child_keyval


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_child_value_float_sp(table, key, val, default, stat, origin)

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

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_child_value_float_dp(table, key, val, default, stat, origin)

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

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_child_value_integer_i1(table, key, val, default, stat, origin)

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

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_child_value_integer_i2(table, key, val, default, stat, origin)

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

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_child_value_integer_i4(table, key, val, default, stat, origin)

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

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_child_value_integer_i8(table, key, val, default, stat, origin)

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

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_child_value_bool(table, key, val, default, stat, origin)

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

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_bool


!> Retrieve TOML value as datetime
subroutine get_child_value_datetime(table, key, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Datetime value
   type(toml_datetime), intent(out) :: val

   !> Default datetime value
   type(toml_datetime), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_datetime


!> Retrieve TOML value as deferred-length character
subroutine get_child_value_string(table, key, val, default, stat, origin)

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

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat, origin)

   if (associated(ptr)) then
      if (allocated(ptr%val)) then
         call get_value(ptr, val, stat, origin)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_string


!> Set TOML value to single precision float
subroutine set_child_value_float_sp(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_float_sp


!> Set TOML value to double precision float
subroutine set_child_value_float_dp(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_float_dp


!> Set TOML value to one byte integer
subroutine set_child_value_integer_i1(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_child_value_integer_i2(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_child_value_integer_i4(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_child_value_integer_i8(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i8


!> Set TOML value to logical
subroutine set_child_value_bool(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_bool


!> Set TOML value to datetime
subroutine set_child_value_datetime(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Datetime value
   type(toml_datetime), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_datetime


!> Set TOML value to deferred-length character
subroutine set_child_value_string(table, key, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat, origin)

   if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_string


end module tomlf_build_table
