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

!> Functions to build a TOML values
!>
!> The build module defines an interface to work with TOML values instead
!> of accessing the raw value directly. Both setter and getter routines defined
!> here are rarely needed in any user context, but serve as a basic building
!> block to define uniform access methods for TOML tables and arrays.
module tomlf_build_keyval
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp, TOML_NEWLINE
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, add_table, add_array, add_keyval, len
   use tomlf_utils, only : toml_raw_to_string, toml_raw_to_float, &
      & toml_raw_to_bool, toml_raw_to_integer, toml_raw_to_timestamp, &
      & toml_raw_verify_string, toml_escape_string
   implicit none
   private

   public :: get_value, set_value


   !> Setter functions to manipulate TOML values
   interface set_value
      module procedure :: set_value_float_sp
      module procedure :: set_value_float_dp
      module procedure :: set_value_integer_i1
      module procedure :: set_value_integer_i2
      module procedure :: set_value_integer_i4
      module procedure :: set_value_integer_i8
      module procedure :: set_value_bool
      module procedure :: set_value_string
   end interface set_value


   !> Getter functions to manipulate TOML values
   interface get_value
      module procedure :: get_value_float_sp
      module procedure :: get_value_float_dp
      module procedure :: get_value_integer_i1
      module procedure :: get_value_integer_i2
      module procedure :: get_value_integer_i4
      module procedure :: get_value_integer_i8
      module procedure :: get_value_bool
      module procedure :: get_value_string
   end interface get_value


   !> Length for the static character variables
   integer, parameter :: buffersize = 128


contains


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


!> Set TOML value to single precision float
subroutine set_value_float_sp(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(es30.6)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
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

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(es30.16)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
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

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
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

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
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

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
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

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
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

   character(len=:), allocatable :: escaped

   if (toml_raw_verify_string(val)) then
      self%raw = val
   else
      call toml_escape_string(val, self%raw, .true.)
   end if

   if (present(stat)) stat = toml_stat%success

end subroutine set_value_string


end module tomlf_build_keyval
