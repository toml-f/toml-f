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

module tomlf_utils
   use tomlf_constants
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time
   use tomlf_utils_convert
   use tomlf_utils_verify
   implicit none
   private

   public :: convert_raw
   public :: toml_raw_to_string, toml_raw_to_float, toml_raw_to_bool
   public :: toml_raw_to_integer, toml_raw_to_timestamp
   public :: toml_raw_verify_string, toml_raw_verify_float, toml_raw_verify_bool
   public :: toml_raw_verify_integer, toml_raw_verify_timestamp
   public :: toml_raw_verify_date, toml_raw_verify_time
   public :: toml_escape_string, toml_get_value_type
   public :: to_string


   interface to_string
      module procedure :: to_string_i1
      module procedure :: to_string_i2
      module procedure :: to_string_i4
      module procedure :: to_string_i8
   end interface to_string


contains


!> Determine TOML value type
function toml_get_value_type(raw) result(vtype)

   !> Raw representation of TOML string
   character(kind=tfc, len=*), intent(in) :: raw

   !> Value type
   integer :: vtype

   if (toml_raw_verify_string(raw)) then
      vtype = toml_type%string
      return
   end if
   if (toml_raw_verify_bool(raw)) then
      vtype = toml_type%boolean
      return
   end if
   if (toml_raw_verify_integer(raw)) then
      vtype = toml_type%int
      return
   end if
   if (toml_raw_verify_float(raw)) then
      vtype = toml_type%float
      return
   end if
   if (toml_raw_verify_timestamp(raw)) then
      vtype = toml_type%datetime
      return
   end if
   vtype = toml_type%invalid

end function


!> Escape all special characters in a TOML string
subroutine toml_escape_string(raw, escaped, multiline)

   !> Raw representation of TOML string
   character(kind=tfc, len=*), intent(in) :: raw

   !> Escaped view of the TOML string
   character(kind=tfc, len=:), allocatable, intent(out) :: escaped

   !> Preserve newline characters
   logical, intent(in), optional :: multiline

   integer :: i
   logical :: preserve_newline

   preserve_newline = .false.
   if (present(multiline)) preserve_newline = multiline

   escaped = '"'
   do i = 1, len(raw)
      select case(raw(i:i))
      case default; escaped = escaped // raw(i:i)
      case('\'); escaped = escaped // '\\'
      case('"'); escaped = escaped // '\"'
      case(TOML_NEWLINE)
         if (preserve_newline) then
            escaped = escaped // raw(i:i)
         else
            escaped = escaped // '\n'
         end if
      case(TOML_FORMFEED); escaped = escaped // '\f'
      case(TOML_CARRIAGE_RETURN); escaped = escaped // '\r'
      case(TOML_TABULATOR); escaped = escaped // '\t'
      case(TOML_BACKSPACE); escaped = escaped // '\b'
      end select
   end do
   escaped = escaped // '"'

end subroutine toml_escape_string


!> Represent an integer as character sequence.
pure function to_string_i1(val) result(string)
   integer, parameter :: ik = tf_i1
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do
   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   string = buffer(pos:)
end function to_string_i1


!> Represent an integer as character sequence.
pure function to_string_i2(val) result(string)
   integer, parameter :: ik = tf_i2
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do
   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   string = buffer(pos:)
end function to_string_i2


!> Represent an integer as character sequence.
pure function to_string_i4(val) result(string)
   integer, parameter :: ik = tf_i4
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do
   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   string = buffer(pos:)
end function to_string_i4


!> Represent an integer as character sequence.
pure function to_string_i8(val) result(string)
   integer, parameter :: ik = tf_i8
   !> Integer value to create string from
   integer(ik), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(ik) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
   end do
   if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   string = buffer(pos:)
end function to_string_i8


end module tomlf_utils
