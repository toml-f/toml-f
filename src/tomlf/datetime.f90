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

!> Implementation of a TOML datetime value
module tomlf_datetime
   use tomlf_constants, only : tfc
   implicit none
   private

   public :: toml_datetime, toml_time, toml_date, to_string, has_date, has_time
   public :: operator(==)


   !> TOML time value (HH:MM:SS.sssssZ...)
   type :: toml_time
      integer :: hour = -1
      integer :: minute = -1
      integer :: second = -1
      integer :: millisec = -1
      character(len=:), allocatable :: zone
   end type

   interface toml_time
      module procedure :: new_toml_time
   end interface toml_time


   !> TOML date value (YYYY-MM-DD)
   type :: toml_date
      integer :: year = -1
      integer :: month = -1
      integer :: day = -1
   end type


   !> TOML datatime value type
   type :: toml_datetime
      type(toml_date) :: date
      type(toml_time) :: time
   end type


   !> Create a new TOML datetime value
   interface toml_datetime
      module procedure :: new_datetime
   end interface toml_datetime


   interface operator(==)
      module procedure :: compare_datetime
   end interface operator(==)


   interface to_string
      module procedure :: to_string_datetime
   end interface to_string


contains


pure function new_datetime(year, month, day, hour, minute, second, millisecond, zone) &
      & result(datetime)
   integer, intent(in), optional :: year
   integer, intent(in), optional :: month
   integer, intent(in), optional :: day
   integer, intent(in), optional :: hour
   integer, intent(in), optional :: minute
   integer, intent(in), optional :: second
   integer, intent(in), optional :: millisecond
   character(len=*), intent(in), optional :: zone
   type(toml_datetime) :: datetime

   if (present(year) .and. present(month) .and. present(day)) then
      datetime%date%year = year
      datetime%date%month = month
      datetime%date%day = day
   end if

   if (present(hour) .and. present(minute) .and. present(second)) then
      datetime%time%hour = hour
      datetime%time%minute = minute
      datetime%time%second = second
      if (present(millisecond)) then
         datetime%time%millisec = millisecond
      end if
      if (present(zone)) then
         datetime%time%zone = zone
      end if
   end if
end function new_datetime


pure function to_string_datetime(datetime) result(str)
   type(toml_datetime), intent(in) :: datetime
   character(kind=tfc, len=:), allocatable :: str

   str = ""
   if (has_date(datetime)) then
      str = str // to_string_date(datetime%date)
   end if

   if (has_time(datetime)) then
      if (has_date(datetime)) then
         str = str // ' '
      end if
      str = str // to_string_time(datetime%time)
   end if
end function to_string_datetime

pure function to_string_date(date) result(str)
   type(toml_date), intent(in) :: date
   character(:, tfc), allocatable :: str

   allocate(character(10, tfc) :: str)
   write(str, '(i4.4,"-",i2.2,"-",i2.2)') &
      &  date%year, date%month, date%day
end function to_string_date

pure function to_string_time(time) result(str)
   type(toml_time), intent(in) :: time
   character(:, tfc), allocatable :: str

   if (time%millisec >= 0) then
      allocate(character(8, tfc) :: str)
      write(str, '(i2.2,":",i2.2,":",i2.2)') &
         &  time%hour, time%minute, time%second
   else
      allocate(character(12, tfc) :: str)
      write(str, '(i2.2,":",i2.2,":",i2.2,".",i3.3)') &
         &  time%hour, time%minute, time%second, time%millisec
   end if
   if (allocated(time%zone)) str = str // trim(time%zone)
end function to_string_time


pure function has_date(datetime)
   class(toml_datetime), intent(in) :: datetime
   logical :: has_date
   has_date = (datetime%date%year >= 0) .and. &
      & (datetime%date%month >= 0) .and. &
      & (datetime%date%day >= 0)
end function has_date

pure function has_time(datetime)
   class(toml_datetime), intent(in) :: datetime
   logical :: has_time
   has_time = (datetime%time%hour >= 0) .and. &
      & (datetime%time%minute >= 0) .and. &
      & (datetime%time%second >= 0)
end function has_time


!> Constructor for toml_time type, necessary due to PGI bug in NVHPC 20.7 and 20.9
elemental function new_toml_time(hour, minute, second, millisec, zone) &
      & result(self)
   integer, intent(in), optional :: hour
   integer, intent(in), optional :: minute
   integer, intent(in), optional :: second
   integer, intent(in), optional :: millisec
   character(len=*), intent(in), optional :: zone
   type(toml_time) :: self
   if (present(hour)) self%hour = hour
   if (present(minute)) self%minute = minute
   if (present(second)) self%second = second
   if (present(millisec)) self%millisec = millisec
   if (present(zone)) self%zone = zone
end function new_toml_time


pure function compare_datetime(lhs, rhs) result(match)
   type(toml_datetime), intent(in) :: lhs
   type(toml_datetime), intent(in) :: rhs
   logical :: match

   match = (has_date(lhs) .eqv. has_date(rhs)) &
      & .and. (has_time(lhs) .eqv. has_time(rhs))
   if (has_date(lhs) .and. has_date(rhs)) then
      match = match .and. compare_date(lhs%date, rhs%date)
   end if

   if (has_time(lhs) .and. has_time(rhs)) then
      match = match .and. compare_time(lhs%time, rhs%time)
   end if
end function compare_datetime


pure function compare_date(lhs, rhs) result(match)
   type(toml_date), intent(in) :: lhs
   type(toml_date), intent(in) :: rhs
   logical :: match

   match = lhs%year == rhs%year .and. lhs%month == rhs%month .and. lhs%day == rhs%day
end function compare_date


pure function compare_time(lhs, rhs) result(match)
   type(toml_time), intent(in) :: lhs
   type(toml_time), intent(in) :: rhs
   logical :: match

   integer :: lms, rms

   lms = max(lhs%millisec, 0)
   rms = max(rhs%millisec, 0)

   match = lhs%hour == rhs%hour .and. lhs%minute == rhs%minute .and. lhs%second == rhs%second &
      & .and. lms == rms .and. allocated(lhs%zone) .eqv. allocated(rhs%zone)

   if (allocated(lhs%zone) .and. allocated(rhs%zone)) then
      match = match .and. lhs%zone == rhs%zone
   end if
end function compare_time


end module tomlf_datetime
