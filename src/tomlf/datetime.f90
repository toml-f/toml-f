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

   public :: toml_datetime, toml_time, toml_date
   public :: operator(==)


   !> TOML time value (HH:MM:SS.sssssZ...)
   type :: toml_time
      integer :: hour = 0
      integer :: minute = 0
      integer :: second = 0
      integer, allocatable :: millisec
      character(len=:), allocatable :: zone
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => time_to_string
   end type

   interface toml_time
      module procedure :: new_toml_time
   end interface toml_time


   !> TOML date value (YYYY-MM-DD)
   type :: toml_date
      integer :: year = 0
      integer :: month = 0
      integer :: day = 0
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => date_to_string
   end type


   !> TOML datatime value type
   type :: toml_datetime
      type(toml_date), allocatable :: date
      type(toml_time), allocatable :: time
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => datetime_to_string
   end type


   interface operator(==)
      module procedure :: compare_datetime
   end interface operator(==)


contains


subroutine date_to_string(lhs, rhs)
   character(kind=tfc, len=:), allocatable, intent(out) :: lhs
   class(toml_date), intent(in) :: rhs
   allocate(character(kind=tfc, len=10) :: lhs)
   write(lhs, '(i4.4,"-",i2.2,"-",i2.2)') &
      &  rhs%year, rhs%month, rhs%day
end subroutine date_to_string


subroutine time_to_string(lhs, rhs)
   character(kind=tfc, len=:), allocatable, intent(out) :: lhs
   class(toml_time), intent(in) :: rhs
   if (allocated(rhs%millisec)) then
      allocate(character(kind=tfc, len=12) :: lhs)
      write(lhs, '(i2.2,":",i2.2,":",i2.2,".",i3.3)') &
         &  rhs%hour, rhs%minute, rhs%second, rhs%millisec
   else
      allocate(character(kind=tfc, len=8) :: lhs)
      write(lhs, '(i2.2,":",i2.2,":",i2.2)') &
         &  rhs%hour, rhs%minute, rhs%second
   end if
   if (allocated(rhs%zone)) lhs = lhs // trim(rhs%zone)
end subroutine time_to_string


subroutine datetime_to_string(lhs, rhs)
   character(kind=tfc, len=:), allocatable, intent(out) :: lhs
   class(toml_datetime), intent(in) :: rhs
   character(kind=tfc, len=:), allocatable :: temporary
   if (allocated(rhs%date)) then
      call rhs%date%to_string(lhs)
      if (allocated(rhs%time)) then
         call rhs%time%to_string(temporary)
         lhs = lhs // tfc_'T' // temporary
      end if
   else
      if (allocated(rhs%time)) lhs = rhs%time
   end if
end subroutine datetime_to_string


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

   match = allocated(lhs%date) .eqv. allocated(rhs%date) &
      & .and. allocated(lhs%time) .eqv. allocated(rhs%time)
   if (allocated(lhs%date) .and. allocated(rhs%date)) then
      match = match .and. compare_date(lhs%date, rhs%date)
   end if

   if (allocated(lhs%time) .and. allocated(rhs%time)) then
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

   lms = 0
   if (allocated(lhs%millisec)) lms = lhs%millisec
   rms = 0
   if (allocated(rhs%millisec)) rms = rhs%millisec

   match = lhs%hour == rhs%hour .and. lhs%minute == rhs%minute .and. lhs%second == rhs%second &
      & .and. lms == rms .and. allocated(lhs%zone) .eqv. allocated(rhs%zone)

   if (allocated(lhs%zone) .and. allocated(rhs%zone)) then
      match = match .and. lhs%zone == rhs%zone
   end if
end function compare_time


end module tomlf_datetime
