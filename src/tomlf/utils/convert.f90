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

!> Contains utilities to convert TOML raw values to actual Fortran datatypes
module tomlf_utils_convert
   use tomlf_constants
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time
   use tomlf_utils_verify
   implicit none
   private

   public :: convert_raw
   public :: toml_raw_to_string, toml_raw_to_float, toml_raw_to_bool
   public :: toml_raw_to_integer, toml_raw_to_timestamp


   !> Overloaded conversion interface
   interface convert_raw
      module procedure :: toml_raw_to_string
      module procedure :: toml_raw_to_float
      module procedure :: toml_raw_to_bool
      module procedure :: toml_raw_to_integer
      module procedure :: toml_raw_to_timestamp
   end interface convert_raw


contains


!> Attempt to convert TOML raw value to Fortran real
function toml_raw_to_float(raw, num) result(stat)

   !> Raw value to convert
   character(kind=tfc, len=*), intent(in) :: raw

   !> Real value represented by raw value
   real(tfr), intent(out) :: num

   !> Status of the evaluation
   logical :: stat

   character(len=len(raw)) :: inp
   integer :: i, j, err

   stat = toml_raw_verify_float(raw)
   if (stat) then
      inp = ''
      j = 0
      do i = 1, len(raw)
         if (raw(i:i) == '_') cycle
         j = j+1
         inp(j:j) = raw(i:i)
      end do
      read(inp, *, iostat=err) num
      stat = err == 0
   end if

end function


!> Attempt to convert TOML raw value to Fortran integer
function toml_raw_to_integer(raw, num) result(stat)

   !> Raw value to convert
   character(kind=tfc, len=*), intent(in) :: raw

   !> Integer value represented by raw value
   integer(tfi), intent(out) :: num

   !> Status of the evaluation
   logical :: stat

   character(kind=tfc, len=len(raw)) :: inp
   character(len=10) :: fmt
   logical :: minus
   integer :: i, j, err
   integer :: first

   stat = toml_raw_verify_integer(raw)
   if (stat) then
      minus = raw(1:1) == '-'
      inp = ''
      first = scan(raw, 'xob')+1
      j = 0
      do i = first, len(raw)
         if (raw(i:i) == '_') cycle
         j = j+1
         inp(j:j) = raw(i:i)
      end do
      if (first > 1) then
         select case(raw(first-1:first-1))
         case('x'); write(fmt, '("(z",i0,")")') j
         case('o'); write(fmt, '("(o",i0,")")') j
         case('b'); write(fmt, '("(b",i0,")")') j
         end select
         read(inp, fmt, iostat=err) num
         if (minus) num = -num
      else
         read(inp, *, iostat=err) num
      end if
      stat = err == 0
   end if

end function toml_raw_to_integer


!> Attempt to convert TOML raw value to Fortran logical
function toml_raw_to_bool(raw, bool) result(stat)

   !> Raw value to convert
   character(kind=tfc, len=*), intent(in) :: raw

   !> Logical value represented by raw value
   logical, intent(out) :: bool

   !> Status of the evaluation
   logical :: stat

   stat = toml_raw_verify_bool(raw)
   if (stat) then
      select case(raw)
      case('true'); bool = .true.
      case('false'); bool = .false.
      end select
   end if

end function toml_raw_to_bool


!> Attempt to convert TOML raw value to TOML datetime
function toml_raw_to_timestamp(raw, timestamp) result(stat)

   !> Raw value to convert
   character(kind=tfc, len=*), intent(in) :: raw

   !> TOML datetime value
   type(toml_datetime), intent(out) :: timestamp

   !> Status of the evaluation
   logical :: stat

   integer :: err, dot_pos, first

   stat = toml_raw_verify_timestamp(raw)
   first = 1
   if (toml_raw_verify_date(raw)) then
      timestamp%date = toml_date()
      read(raw(1:4), *, iostat=err) timestamp%date%year
      stat = err == 0
      read(raw(6:7), *, iostat=err) timestamp%date%month
      stat = stat .and. err == 0
      read(raw(9:10), *, iostat=err) timestamp%date%day
      stat = stat .and. err == 0
      if (.not.stat .or. len(raw) == 10) return
      first = 12
   end if

   if (toml_raw_verify_time(raw(first:))) then
      timestamp%time = toml_time()
      read(raw(first:first+1), *, iostat=err) timestamp%time%hour
      stat = err == 0
      read(raw(first+3:first+4), *, iostat=err) timestamp%time%minute
      stat = stat .and. err == 0
      read(raw(first+6:first+7), *, iostat=err) timestamp%time%second
      stat = stat .and. err == 0
      if (len(raw(first:)) > 8) then
         dot_pos = index(raw, '.')
         if (dot_pos > 0) then
            allocate(timestamp%time%millisec, source=0)
            read(raw(dot_pos+1:dot_pos+3), *, iostat=err) timestamp%time%millisec
            stat = stat .and. err == 0
         end if
         dot_pos = verify(raw(first:), TOML_DIGITS//'.:') + first - 1
         if (dot_pos > first) timestamp%time%zone = raw(dot_pos:)
      end if
   end if

end function toml_raw_to_timestamp


logical function toml_raw_to_string(raw, str) result(stat)

   character(kind=tfc, len=*), intent(in) :: raw
   character(kind=tfc, len=:), allocatable, intent(out) :: str
   character(kind=tfc, len=:), allocatable :: tmp
   logical :: multiline
   logical :: verbatim

   stat = toml_raw_verify_string(raw)
   if (stat) then
      verbatim = raw(1:1) == TOML_SQUOTE
      multiline = verify(raw(1:3), TOML_DQUOTE) == 0 &
         &   .or. verify(raw(1:3), TOML_SQUOTE) == 0
      if (multiline) then
         tmp = raw(4:len(raw)-3)
         call toml_normalize_multiline(tmp)
      else
         tmp = raw(2:len(raw)-1)
      end if
      if (.not.verbatim) call toml_normalize_string(tmp)
      call move_alloc(tmp, str)
   end if

end function toml_raw_to_string


subroutine toml_normalize_multiline(str)

   character(kind=tfc, len=:), allocatable, intent(inout) :: str
   character(kind=tfc, len=:), allocatable :: tmp
   integer :: i, j, bsl

   ! if there are no newlines, we are done here
   if (scan(str, TOML_NEWLINE) == 0) return
   ! check for leading newlines
   i = verify(str, TOML_NEWLINE)
   ! for the case everything is newline, we will go with an empty string
   if (i == 0) then
      str = ''
      return
   end if
   tmp = ''
   do while (i < len(str))
      ! find next backslash character
      bsl = index(str(i:), '\') - 1
      if (bsl < 0) then
         tmp = tmp // str(i:)
         i = len(str)
      else
         j = verify(str(i+bsl+1:), TOML_WHITESPACE//TOML_NEWLINE) - 1
         if (j < 0) then
            tmp = tmp // str(i:i+bsl)
            i = len(str)
         else if (j == 0) then
            tmp = tmp // str(i:i+bsl)
            i = i + bsl + 1
         else
            tmp = tmp // str(i:i+bsl-1)
            i = i + bsl + j + 1
         end if
      end if
   end do
   call move_alloc(tmp, str)

end subroutine toml_normalize_multiline


subroutine toml_normalize_string(str)
   character(kind=tfc, len=:), allocatable, intent(inout) :: str
   character(kind=tfc, len=:), allocatable :: tmp
   character :: ch
   integer :: i, ii
   logical :: escape
   integer, parameter :: x00 = int(z'00'), x08 = int(z'08'), x0b = int(z'0B'), &
      & x1f = int(z'1f'), x7f = int(z'7f')

   escape = .false.
   tmp = ''
   do i = 1, len(str)
      ch = str(i:i)
      ii = ichar(ch)
      if (escape) then
         escape = .false.
         select case(ch)
         case('b'); tmp = tmp // TOML_BACKSPACE
         case('t'); tmp = tmp // TOML_TABULATOR
         case('n'); tmp = tmp // TOML_NEWLINE
         case('f'); tmp = tmp // TOML_FORMFEED
         case('r'); tmp = tmp // TOML_CARRIAGE_RETURN
         case('"', '\'); tmp = tmp // ch
         case('u'); tmp = tmp // '\u' ! FIXME
         case('U'); tmp = tmp // '\U' ! FIXME
         end select
      else
         if (ch == '\') then
            escape = .true.
         else
            ! check for illegal control characters
            if ((x00 <= ii .and. ii <= x08) .or. &
               &(x0B <= ii .and. ii <= x1f) .or. ii == x7f) return
            tmp = tmp // ch
         end if
      end if
   end do
   call move_alloc(tmp, str)

end subroutine toml_normalize_string


end module tomlf_utils_convert
