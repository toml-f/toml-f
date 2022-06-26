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

!> Check for consistency of the various version strings around
program tftest_fpm
   use, intrinsic :: iso_fortran_env, only : output_unit, error_unit
   use tomlf
   use tomlf_build
   implicit none
   integer :: length
   character(len=:), allocatable :: argument, version_string
   type(toml_table), allocatable :: table
   integer :: unit
   logical :: exist, match

   call get_tomlf_version(string=version_string)
   if (abs(command_argument_count()) == 1) then
      call get_command_argument(1, length=length)
      allocate(character(len=length) :: argument)
      call get_command_argument(1, argument)
   else
      argument = 'fpm.toml'
   end if
   inquire(file=argument, exist=exist)
   if (.not.exist) then
      write(error_unit, '(a, ":", 1x, a)') &
         & "Could not find file", argument
      error stop
   end if
   open(newunit=unit, file=argument)
   call toml_parse(table, unit)
   close(unit)
   if (.not.allocated(table)) then
      write(error_unit, '(a, 1x, a)') &
         & argument, "could not be parsed, check format and/or implementation"
      error stop
   end if
   call get_value(table, "version", argument)

   match = argument == version_string

   if (.not.match) then
      write(error_unit, '(a)') &
         & "Internal version and provided version do not match!"
      write(error_unit, '(a, ":", 1x, a)') &
         & "provided", argument, "internal", version_string
      error stop
   end if

end program tftest_fpm
