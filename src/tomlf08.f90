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

!> User interface to TOML-Fortran. Provides most elementar functions and
!  wrappers to hide internals of the parser.
module tomlf08
   use tomlf08_type, only: toml_table
   use tomlf08_ser, only: toml_serializer
   use tomlf08_de, only: toml_string_deserializer
   implicit none
   private
   public :: toml_parse
   public :: toml_serializer
   public :: toml_table

   interface toml_parse
      module procedure :: toml_parse_unit
      module procedure :: toml_parse_string
   end interface toml_parse

contains

subroutine tomlf08_version(vstr)
   character(len=:), allocatable, intent(out) :: vstr
   vstr = "0.1"
end subroutine tomlf08_version

!> Parse a TOML input from a given IO unit.
subroutine toml_parse_unit(table, unit, iostat)
   use iso_fortran_env
   use tomlf08_constants, only: TOML_NEWLINE
   type(toml_table), allocatable, intent(out) :: table
   integer, intent(in) :: unit
   integer, intent(out), optional :: iostat
   character(len=:), allocatable :: conf
   integer, parameter :: bufsize = 512
   character(len=bufsize) :: buffer
   integer :: size
   integer :: error
   allocate(character(len=0) :: conf)
   do 
      read(unit, '(a)', advance='no', iostat=error, size=size) buffer
      if (error > 0) exit
      conf = conf // buffer(:size)
      if (error < 0) then
         if (is_iostat_eor(error)) then
            error = 0
            conf = conf // TOML_NEWLINE
         end if
         if (is_iostat_end(error)) then
            error = 0
            exit
         end if
      end if
   end do

   if (error /= 0) then
      if (present(iostat)) iostat = error
      return
   end if

   call toml_parse_string(table, conf)

   if (.not.allocated(table) .and. present(iostat)) iostat = 1

end subroutine toml_parse_unit

!> Wrapper to parse a TOML string.
subroutine toml_parse_string(table, conf)
   use iso_fortran_env, only: error_unit
   type(toml_table), allocatable, intent(out) :: table
   character(len=*), intent(in), target :: conf
   type(toml_string_deserializer) :: de

   de = toml_string_deserializer(conf)

   call de%parse

   if (allocated(de%error)) then
      write(error_unit, '("line",1x,i0,":",1x,a)') &
         &  de%error%lineno, de%error%message
      return
   end if

   call move_alloc(de%root, table)

end subroutine toml_parse_string

end module tomlf08
