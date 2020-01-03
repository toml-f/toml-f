! This file is part of toml-f.
!
! Copyright (C) 2019-2020 Sebastian Ehlert
!
! toml-f is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! toml-f is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with toml-f.  If not, see <https://www.gnu.org/licenses/>.

!> Utilities to work with TOML data types.
module tomlf08_utils
   use tomlf08_constants
   implicit none

contains

integer(toml_type) function toml_get_value_type(raw) result(vtype)
   character(len=*), intent(in) :: raw
   if (toml_raw_verify_string(raw)) then
      vtype = STRING_TYPE
      return
   end if
   if (toml_raw_verify_bool(raw)) then
      vtype = BOOL_TYPE
      return
   end if
   if (toml_raw_verify_integer(raw)) then
      vtype = INTEGER_TYPE
      return
   end if
   if (toml_raw_verify_float(raw)) then
      vtype = FLOAT_TYPE
      return
   end if
   if (toml_raw_verify_timestamp(raw)) then
      vtype = TIMESTAMP_TYPE
      return
   end if
   vtype = INVALID_TYPE
end function

logical function toml_raw_to_string(raw, str) result(stat)
   character(len=*), intent(in) :: raw
   character(len=:), allocatable, intent(out) :: str
   character(len=:), allocatable :: tmp
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
end function

subroutine toml_normalize_multiline(str)
   character(len=:), allocatable, intent(inout) :: str
   character(len=:), allocatable :: tmp
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
   character(len=:), allocatable, intent(inout) :: str
   character(len=:), allocatable :: tmp
   character :: ch
   integer :: i, ii
   logical :: escape
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
            if ((z'00' <= ii .and. ii <= z'08') .or. &
               &(z'0B' <= ii .and. ii <= z'1f') .or. ii == z'7f') return
            tmp = tmp // ch
         end if
      end if
   end do
   call move_alloc(tmp, str)
end subroutine toml_normalize_string

subroutine toml_escape_string(raw, escaped)
   character(len=*), intent(in) :: raw
   character(len=:), allocatable, intent(out) :: escaped
   integer :: i
   escaped = ''
   do i = 1, len(raw)
      select case(raw(i:i))
      case default; escaped = escaped // raw(i:i)
      case('\'); escaped = escaped // '\\'
      case('"'); escaped = escaped // '\"'
      case(TOML_NEWLINE); escaped = escaped // '\n'
      case(TOML_FORMFEED); escaped = escaped // '\f'
      case(TOML_CARRIAGE_RETURN); escaped = escaped // '\r'
      case(TOML_TABULATOR); escaped = escaped // '\t'
      case(TOML_BACKSPACE); escaped = escaped // '\b'
      end select
   end do
end subroutine toml_escape_string

logical elemental function toml_raw_verify_string(raw) result(stat)
   character(len=*), intent(in) :: raw
   stat = raw(1:1) == TOML_SQUOTE .or. raw(1:1) == TOML_DQUOTE
end function

logical function toml_raw_to_float(raw, num) result(stat)
   character(len=*), intent(in) :: raw
   character(len=len(raw)) :: inp
   real(TOML_FLOAT_KIND), intent(out) :: num
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

logical elemental function toml_raw_verify_float(raw) result(stat)
   character(len=*), intent(in) :: raw
   logical :: plus_minus
   integer :: first
   integer :: dot_pos
   integer :: exp_pos
   first = 1
   stat = .false.
   if (raw == 'nan') then
      stat = .true.
      return
   end if
   ! allow leading + or -
   plus_minus = raw(1:1) == '+' .or. raw(1:1) == '-'
   if (plus_minus) first = first+1
   ! allow infinity and not-a-number
   if (raw(first:) == 'inf' .or. raw(first:) == 'nan') then
      stat = .true.
      return
   end if
   ! position of dot and exponent
   dot_pos = index(raw, '.')
   exp_pos = scan(raw, 'Ee')
   if (dot_pos > 0 .and. exp_pos > 0 .and. dot_pos > exp_pos) return
   ! check for leading underscores
   if (raw(first:first) == '_') return
   ! check for leading dots
   if (first == dot_pos) return
   ! zero must be followed by a dot or exponent
   if (raw(first:first) == '0' .and. len(raw(first:)) > 1) then
      if (first+1 /= dot_pos .and. first+1 /= exp_pos) return
   end if
   ! no double underscores
   if (index(raw, '__') > 0) return
   ! check for digits
   stat = verify(raw(first:), TOML_DIGITS//'._-+eE') == 0

end function

logical function toml_raw_to_integer(raw, num) result(stat)
   character(len=*), intent(in) :: raw
   character(len=len(raw)) :: inp
   character(len=10) :: fmt
   integer(TOML_INTEGER_KIND), intent(out) :: num
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
end function

logical elemental function toml_raw_verify_integer(raw) result(stat)
   character(len=*), intent(in) :: raw
   logical :: plus_minus
   integer :: first
   integer :: base
   first = 1
   base = 10
   stat = .false.
   ! allow leading + or -
   plus_minus = raw(1:1) == '+' .or. raw(1:1) == '-'
   if (plus_minus) first = first+1
   ! check for leading underscores
   if (raw(first:first) == '_') return
   ! no double underscores
   if (index(raw, '__') > 0) return
   ! 0 indicates other base systems
   if (raw(first:first) == '0' .and. len(raw) > first) then
      select case(raw(first+1:first+1))
      case('x'); base = 16
      case('o'); base = 8
      case('b'); base = 2
      case default; return ! disallow 0[0-9_]+
      end select
      first = first + 2
      ! check for leading underscores, again
      if (raw(first:first) == '_') return
   end if
   ! check for trailing underscores
   if (raw(len(raw):) == '_') return
   ! verify we only allowed digits
   select case(base)
   case default
      stat = verify(raw(first:), TOML_DIGITS//'_') == 0
   case(16)
      stat = verify(raw(first:), TOML_HEXDIGITS//'_') == 0
   case(8)
      stat = verify(raw(first:), TOML_OCTDIGITS//'_') == 0
   case(2)
      stat = verify(raw(first:), TOML_BINDIGITS//'_') == 0
   end select
end function

logical function toml_raw_to_bool(raw, bool) result(stat)
   character(len=*), intent(in) :: raw
   logical(TOML_BOOL_KIND), intent(out) :: bool
   stat = toml_raw_verify_bool(raw)
   if (stat) then
      select case(raw)
      case('true'); bool = .true._TOML_BOOL_KIND
      case('false'); bool = .false._TOML_BOOL_KIND
      end select
   end if
end function

logical elemental function toml_raw_verify_bool(raw) result(stat)
   character(len=*), intent(in) :: raw
   stat = raw == 'true' .or. raw == 'false'
end function

logical function toml_raw_to_timestamp(raw, timestamp) result(stat)
   character(len=*), intent(in) :: raw
   type(toml_datetime), intent(out) :: timestamp
   integer :: err, dot_pos, first
   stat = toml_raw_verify_timestamp(raw)
   first = 1
   if (toml_raw_verify_date(raw)) then
      allocate(timestamp%date, source=toml_date())
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
      allocate(timestamp%time, source=toml_time())
      read(raw(first:first+1), *, iostat=err) timestamp%time%hour
      stat = err == 0
      read(raw(first+3:first+4), *, iostat=err) timestamp%time%minute
      stat = stat .and. err == 0
      read(raw(first+6:first+7), *, iostat=err) timestamp%time%second
      stat = stat .and. err == 0
      if (len(raw(first:)) > 8) then
         dot_pos = index(raw, '.')
         if (dot_pos == 9) then
            allocate(timestamp%time%millisec, source=0)
            read(raw(dot_pos+1:dot_pos+3), *, iostat=err) timestamp%time%millisec
            stat = stat .and. err == 0
         end if
         dot_pos = verify(raw(first:), TOML_DIGITS//'.:') + first - 1
         if (dot_pos > first) timestamp%time%zone = raw(dot_pos:)
      end if
   end if
end function

logical elemental function toml_raw_verify_timestamp(raw) result(stat)
   character(len=*), intent(in) :: raw
   integer :: first
   first = 1
   stat = .false.
   if (toml_raw_verify_date(raw)) then
      if (len(raw) == 10) then
         stat = .true.
         return
      end if
      if (raw(11:11) /= ' ' .and. raw(11:11) /= 'T') return
      first = 12
   end if

   stat = toml_raw_verify_time(raw(first:))
end function

logical elemental function toml_raw_verify_date(raw) result(stat)
   character(len=*), intent(in) :: raw
   stat = .false.
   ! YYYY-MM-DD
   if (len(raw) >= 10) then
      stat = verify(raw(1:4), TOML_DIGITS) == 0 .and. raw(5:5) == '-' .and. &
         &   verify(raw(6:7), TOML_DIGITS) == 0 .and. raw(8:8) == '-' .and. &
         &   verify(raw(9:10), TOML_DIGITS) == 0
   end if
end function

logical elemental function toml_raw_verify_time(raw) result(stat)
   character(len=*), intent(in) :: raw
   stat = .false.
   ! HH:MM:SS
   if (len(raw) >= 8) then
      stat = verify(raw(1:2), TOML_DIGITS) == 0 .and. raw(3:3) == ':' .and. &
         &   verify(raw(4:5), TOML_DIGITS) == 0 .and. raw(6:6) == ':' .and. &
         &   verify(raw(7:8), TOML_DIGITS) == 0
   end if
end function

end module
