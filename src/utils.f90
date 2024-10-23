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
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time, to_string
   use tomlf_utils_io, only : read_whole_file, read_whole_line
   implicit none
   private

   public :: toml_escape_string
   public :: to_string
   public :: read_whole_file, read_whole_line


   interface to_string
      module procedure :: to_string_i1
      module procedure :: to_string_i2
      module procedure :: to_string_i4
      module procedure :: to_string_i8
      module procedure :: to_string_r8
   end interface to_string


contains


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
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
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
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
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
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
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
   character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

   if (val == 0_ik) then
      string = numbers(0)
      return
   end if

   n = sign(val, -1_ik)
   buffer = ""
   pos = buffer_len + 1
   do while (n < 0_ik)
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

!> Represent an real as character sequence.
pure function to_string_r8(val) result(string)
   integer, parameter :: rk = tfr
   !> Real value to create string from
   real(rk), intent(in) :: val
   !> String representation of integer
   character(len=:), allocatable :: string

   character(128, tfc) :: buffer

   if (val > huge(val)) then
      string = "+inf"
   else if (val < -huge(val)) then
      string = "-inf"
   else if (val /= val) then
      string = "nan"
   else
      if (abs(val) >= 1.0e+100_rk) then
         write(buffer, '(es24.16e3)') val
      else if (abs(val) >= 1.0e+10_rk) then
         write(buffer, '(es24.16e2)') val
      else if (abs(val) >= 1.0e+3_rk) then
         write(buffer, '(es24.16e1)') val
      else
         write(buffer, '(f24.16)') val
      end if
      string = trim(adjustl(buffer))
   end if
end function to_string_r8

end module tomlf_utils
