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

!> Example executable to read and emit a TOML document
program toml2json
   use, intrinsic :: iso_fortran_env, only : input_unit
   use tomlf
   use tftest_json_ser
   implicit none
   integer :: iarg, length
   character(len=:), allocatable :: argument
   type(toml_table), allocatable :: table
   type(json_serializer) :: ser
   integer :: unit
   logical :: exist

   ser%indentation = "  "

   if (command_argument_count() > 0) then
      do iarg = 1, command_argument_count()
         if (allocated(argument)) deallocate(argument)
         call get_command_argument(iarg, length=length)
         allocate(character(len=length) :: argument)
         exist = length > 0
         if (exist) then
            call get_command_argument(iarg, argument)
            inquire(file=argument, exist=exist)
         end if
         if (exist) then
            open(newunit=unit, file=argument)
            if (allocated(table)) deallocate(table)
            call toml_parse(table, unit)
            close(unit)
            if (allocated(table)) then
               call table%accept(ser)
               call table%destroy
            end if
         end if
      end do
   else
      call toml_parse(table, input_unit)
      if (allocated(table)) then
         call table%accept(ser)
         call table%destroy
      else
         error stop
      end if
   end if
   if (allocated(argument)) deallocate(argument)

end program toml2json
