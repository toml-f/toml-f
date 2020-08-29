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


   !> Setter functions to manipulate TOML tables
   !interface set_value
   !end interface set_value


   !> Getter functions to manipulate TOML tables
   interface get_value
      module procedure :: get_elem_table
      module procedure :: get_elem_array
      module procedure :: get_elem_keyval
      module procedure :: get_elem_value_string
   end interface get_value


contains


subroutine get_elem_table(array, pos, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Pointer to child table
   class(toml_table), pointer, intent(out) :: ptr

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
   class(toml_array), pointer, intent(out) :: ptr

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
   class(toml_keyval), pointer, intent(out) :: ptr

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

   class(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_string


end module tomlf_build_array
