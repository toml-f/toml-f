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
program tftest_version
   use, intrinsic :: iso_fortran_env, only : output_unit, error_unit
   use tomlf
   implicit none
   integer :: length, major, minor, patch
   logical :: match
   character(len=:), allocatable :: argument, version_string

   call get_tomlf_version(major, minor, patch, string=version_string)
   if (command_argument_count() == 1) then
      call get_command_argument(1, length=length)
      allocate(character(len=length) :: argument)
      call get_command_argument(1, argument)
      match = argument == version_string
      write(version_string, '(*(i0:, "."))') major, minor, patch
      match = argument == version_string .and. match
      if (.not.match) then
         write(error_unit, '(a)') &
            & "Internal version and provided version do not match!"
         write(error_unit, '(a, ":", 1x, a)') &
            & "provided", argument, "internal", version_string
         error stop
      end if
   else
      write(output_unit, '(a)') version_string
   end if

end program tftest_version
