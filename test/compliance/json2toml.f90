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
program json2toml
   use, intrinsic :: iso_fortran_env, only : input_unit, output_unit
   use tomlf
   use tjson_parser
   implicit none
   integer :: iarg, length
   character(len=:), allocatable :: argument
   class(toml_value), allocatable :: object
   type(toml_error), allocatable :: error
   logical :: exist

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
            if (allocated(object)) deallocate(object)
            call json_load(object, argument, error=error)
            if (allocated(error)) print '(a)', error%message
            if (allocated(object)) then
               call toml_dump(object, output_unit, error)
               call object%destroy
            end if
         end if
      end do
   else
      call json_load(object, input_unit, error=error)
      if (allocated(error)) print '(a)', error%message
      if (allocated(object)) then
         call toml_dump(object, output_unit, error)
         call object%destroy
      else
         error stop
      end if
   end if
   if (allocated(argument)) deallocate(argument)

end program json2toml
