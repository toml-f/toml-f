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

!> Contains utilities to verify TOML raw values correspond to a certain datatype
module tomlf_utils_verify
   use tomlf_constants
   implicit none
   private

   public :: toml_raw_verify_string, toml_raw_verify_float, toml_raw_verify_bool
   public :: toml_raw_verify_integer, toml_raw_verify_timestamp
   public :: toml_raw_verify_date, toml_raw_verify_time


contains


!> Verify a raw value as TOML string
pure function toml_raw_verify_string(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   stat = raw(1:1) == TOML_SQUOTE .or. raw(1:1) == TOML_DQUOTE

end function toml_raw_verify_string


!> Verify a raw value as TOML float
pure function toml_raw_verify_float(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

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
   if (dot_pos == 0 .and. exp_pos == 0) return
   if (dot_pos > 0 .and. exp_pos > 0 .and. dot_pos > exp_pos) return
   ! check for leading or trailing underscores
   if (raw(first:first) == '_' .or. raw(len(raw):) == '_') return
   ! check for leading or trailing dots
   if (first == dot_pos .or. len(raw) == dot_pos) return
   if (dot_pos > 0) then
      if (raw(dot_pos+1:dot_pos+1) == '_' .or. raw(dot_pos-1:dot_pos-1) == '_') return
   end if
   ! zero must be followed by a dot or exponent
   if (raw(first:first) == '0' .and. len(raw(first:)) > 1) then
      if (first+1 /= dot_pos .and. first+1 /= exp_pos) return
   end if
   ! no double underscores
   if (index(raw, '__') > 0) return
   ! check for digits
   stat = verify(raw(first:), TOML_DIGITS//'._-+eE') == 0

end function toml_raw_verify_float


!> Verify a raw value as TOML integer
pure function toml_raw_verify_integer(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   logical :: plus_minus
   integer :: first, base

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


!> Verify a raw value as TOML bool
pure function toml_raw_verify_bool(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   stat = raw == 'true' .or. raw == 'false'

end function toml_raw_verify_bool


!> Verify a raw value as TOML datetime expression
pure function toml_raw_verify_timestamp(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

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

end function toml_raw_verify_timestamp


!> Verify a raw value as TOML date expression (YYYY-MM-DD)
pure function toml_raw_verify_date(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   if (len(raw) >= 10) then
      stat = verify(raw(1:4), TOML_DIGITS) == 0 .and. raw(5:5) == '-' .and. &
         &   verify(raw(6:7), TOML_DIGITS) == 0 .and. raw(8:8) == '-' .and. &
         &   verify(raw(9:10), TOML_DIGITS) == 0
   else
      stat = .false.
   end if

end function toml_raw_verify_date


!> Verify a raw value as TOML time expression (HH:MM:SS)
pure function toml_raw_verify_time(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   if (len(raw) >= 8) then
      stat = verify(raw(1:2), TOML_DIGITS) == 0 .and. raw(3:3) == ':' .and. &
         &   verify(raw(4:5), TOML_DIGITS) == 0 .and. raw(6:6) == ':' .and. &
         &   verify(raw(7:8), TOML_DIGITS) == 0
   else
      stat = .false.
   end if

end function toml_raw_verify_time


end module tomlf_utils_verify
