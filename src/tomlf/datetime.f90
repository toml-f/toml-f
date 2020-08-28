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

!> Implementation of a TOML datetime value
module tomlf_datetime
   use tomlf_constants, only : tfc
   implicit none
   private

   public :: toml_datetime, toml_time, toml_date


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


end module tomlf_datetime
