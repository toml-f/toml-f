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
      integer :: msec = -1
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
      module procedure :: new_datetime_from_string
   end interface toml_datetime


   interface operator(==)
      module procedure :: compare_datetime
   end interface operator(==)


   interface to_string
      module procedure :: to_string_datetime
   end interface to_string


contains


pure function new_datetime(year, month, day, hour, minute, second, msecond, zone) &
      & result(datetime)
   integer, intent(in), optional :: year
   integer, intent(in), optional :: month
   integer, intent(in), optional :: day
   integer, intent(in), optional :: hour
   integer, intent(in), optional :: minute
   integer, intent(in), optional :: second
   integer, intent(in), optional :: msecond
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
      if (present(msecond)) then
         datetime%time%msec = msecond
      end if
      if (present(zone)) then
         datetime%time%zone = zone
      end if
   end if
end function new_datetime


pure function new_datetime_from_string(string) result(datetime)
   character(len=*), intent(in) :: string
   type(toml_datetime) :: datetime

   type(toml_date) :: date
   type(toml_time) :: time

   integer :: it, tmp, first
   character(*, tfc), parameter :: num = "0123456789"
   integer, allocatable :: msec(:)

   first = 0

   if (all([string(first+5:first+5), string(first+8:first+8)] == "-")) then
      date%year = 0
      do it = first + 1, first + 4
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         date%year = date%year * 10 + tmp
      end do

      date%month = 0
      do it = first + 6, first + 7
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         date%month = date%month * 10 + tmp
      end do

      date%day = 0
      do it = first + 9, first + 10
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         date%day = date%day * 10 + tmp
      end do

      first = first + 11
      datetime%date = date
   end if

   if (all([string(first+3:first+3), string(first+6:first+6)] == ":") &
      & .and. first < len(string)) then
      time%hour = 0
      do it = first + 1, first + 2
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         time%hour = time%hour * 10 + tmp
      end do

      time%minute = 0
      do it = first + 4, first + 5
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         time%minute = time%minute * 10 + tmp
      end do

      time%second = 0
      do it = first + 7, first + 8
         tmp = scan(num, string(it:it)) - 1
         if (tmp < 0) exit
         time%second = time%second * 10 + tmp
      end do

      first = first + 8
      if (string(first+1:first+1) == ".") then
         msec = [integer::]
         do it = first + 2, len(string)
            tmp = scan(num, string(it:it)) - 1
            if (tmp < 0) exit
            msec = [msec, tmp]
         end do
         first = it - 1

         msec = [msec, 0, 0, 0, 0, 0, 0]
         time%msec = sum(msec(1:6) * [100000, 10000, 1000, 100, 10, 1])
      end if

      if (first < len(string)) then
         time%zone = ""
         do it = first + 1, len(string)
            time%zone = time%zone // string(it:it)
         end do
         if (time%zone == "z") time%zone = "Z"
      end if
      datetime%time = time
   end if

end function new_datetime_from_string


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

   integer :: msec, width
   character(1), parameter :: places(6) = ["1", "2", "3", "4", "5", "6"]

   if (time%msec < 0) then
      allocate(character(8, tfc) :: str)
      write(str, '(i2.2,":",i2.2,":",i2.2)') &
         &  time%hour, time%minute, time%second
   else
      width = 6
      msec = time%msec
      do while(mod(msec, 10) == 0 .and. width > 3)
         width = width - 1
         msec = msec / 10
      end do
      allocate(character(9 + width, tfc) :: str)
      write(str, '(i2.2,":",i2.2,":",i2.2,".",i'//places(width)//'.'//places(width)//')') &
         &  time%hour, time%minute, time%second, msec
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
elemental function new_toml_time(hour, minute, second, msec, zone) &
      & result(self)
   integer, intent(in), optional :: hour
   integer, intent(in), optional :: minute
   integer, intent(in), optional :: second
   integer, intent(in), optional :: msec
   character(len=*), intent(in), optional :: zone
   type(toml_time) :: self
   if (present(hour)) self%hour = hour
   if (present(minute)) self%minute = minute
   if (present(second)) self%second = second
   if (present(msec)) self%msec = msec
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

   lms = max(lhs%msec, 0)
   rms = max(rhs%msec, 0)

   match = lhs%hour == rhs%hour .and. lhs%minute == rhs%minute .and. lhs%second == rhs%second &
      & .and. lms == rms .and. allocated(lhs%zone) .eqv. allocated(rhs%zone)

   if (allocated(lhs%zone) .and. allocated(rhs%zone)) then
      match = match .and. lhs%zone == rhs%zone
   end if
end function compare_time


end module tomlf_datetime
