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

!> Example application to show the usage of TOML-Fortran to read, modify and
!  parse TOML data structures.
program tomlf_example
   use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
   use pkg
   implicit none
   integer :: iarg, length
   character(len=:), allocatable :: argument
   type(package_data) :: pkg_data
   type(error_data), allocatable :: error
   logical :: exist

   if (command_argument_count() > 0) then
      do iarg = 1, command_argument_count()
         if (allocated(argument)) deallocate(argument)
         call get_command_argument(iarg, length=length)
         allocate(character(len=length) :: argument)
         exist = length > 0
         if (exist) then
            call get_command_argument(iarg, argument)
            write(output_unit, '(a, 1x, a, 1x, a)') &
               "Collecting meta data from", argument, "..."
            call get_package_data(pkg_data, argument, error)
            if (allocated(error)) then
               write(error_unit, '(a, 1x, a, /, a)') &
                  & "Error while processing", argument, error%message
               error stop 1
            end if
            call pkg_data%info(output_unit)
         end if
      end do
   end if
   if (allocated(argument)) deallocate(argument)

end program tomlf_example
