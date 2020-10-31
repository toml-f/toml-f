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

!> Functions to build TOML arrays.
!>
!> This build module defines a high level interface to work with TOML arrays
!> and construct them in a convenient way.
!>
!> The access to the array elements happens by position in the array, the indexing
!> is one based, following the language convention of Fortran. All functions
!> will only allow access of elements within the bounds of the array, specifying
!> indices out-of-bounds should be save, as it only sets the status of operation.
!> The getter functions allow access to other tables and arrays as well as
!> convenient wrappers to retrieve value data
!>
!> The setter functions are somewhat weaker compared to the setter functions
!> available for TOML tables. To limit the potential havoc this routines can
!> cause they can only access the array within its bounds. Setting a value to
!> another value will overwrite it, while setting a value to a table or an array
!> will fail, for safety reasons.
!>
!> To (re)build an array appending to it is the best choice, tables and arrays
!> should always be create by using the corresponding `add_table` and `add_array`
!> function. While this can become cumbersome for values, the setter routines
!> allow out-of-bound access to for the next element in an array and will indeed
!> just append a new value to it.
module tomlf_build_array
   use tomlf_build_keyval, only : get_value, set_value
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, add_table, add_array, add_keyval, len
   use tomlf_utils, only : toml_raw_to_string, toml_raw_to_float, &
      & toml_raw_to_bool, toml_raw_to_integer, toml_raw_to_timestamp
   implicit none
   private

   public :: get_value, set_value


   !> Setter functions to manipulate TOML arrays
   interface set_value
      module procedure :: set_elem_value_string
      module procedure :: set_elem_value_float_sp
      module procedure :: set_elem_value_float_dp
      module procedure :: set_elem_value_int_i1
      module procedure :: set_elem_value_int_i2
      module procedure :: set_elem_value_int_i4
      module procedure :: set_elem_value_int_i8
      module procedure :: set_elem_value_bool
   end interface set_value


   !> Getter functions to manipulate TOML arrays
   interface get_value
      module procedure :: get_elem_table
      module procedure :: get_elem_array
      module procedure :: get_elem_keyval
      module procedure :: get_elem_value_string
      module procedure :: get_elem_value_float_sp
      module procedure :: get_elem_value_float_dp
      module procedure :: get_elem_value_int_i1
      module procedure :: get_elem_value_int_i2
      module procedure :: get_elem_value_int_i4
      module procedure :: get_elem_value_int_i8
      module procedure :: get_elem_value_bool
   end interface get_value


contains


subroutine get_elem_table(array, pos, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp

   nullify(ptr)

   call array%get(pos, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_table)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_table


subroutine get_elem_array(array, pos, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp

   nullify(ptr)

   call array%get(pos, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_array)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_array


subroutine get_elem_keyval(array, pos, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp

   nullify(ptr)

   call array%get(pos, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_keyval)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_keyval


!> Retrieve TOML value as deferred-length character
subroutine get_elem_value_string(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_string


!> Retrieve TOML value as single precision floating point number
subroutine get_elem_value_float_sp(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_sp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_float_sp


!> Retrieve TOML value as double precision floating point number
subroutine get_elem_value_float_dp(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_dp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_float_dp


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i1(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i1


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i2(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i2


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i4(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i4


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i8(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i8


!> Retrieve TOML value as boolean
subroutine get_elem_value_bool(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   logical, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_bool


!> Retrieve TOML value as deferred-length character
subroutine set_elem_value_string(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_string


!> Retrieve TOML value as single precision floating point number
subroutine set_elem_value_float_sp(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_float_sp


!> Retrieve TOML value as double precision floating point number
subroutine set_elem_value_float_dp(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_float_dp


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i1(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i1


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i2(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i2


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i4(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i4


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i8(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i8


!> Retrieve TOML value as boolean value
subroutine set_elem_value_bool(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_bool


end module tomlf_build_array
