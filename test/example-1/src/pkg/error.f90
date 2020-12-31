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

!> Implementation of basic error handling
module pkg_error
   implicit none
   private

   public :: error_data
   public :: fatal_error, file_not_found_error


   !> Data type defining an error
   type :: error_data

      !> Error message
      character(len=:), allocatable :: message

   end type error_data


contains


!> Generic fatal runtime error
subroutine fatal_error(error, message)

   !> Instance of the error data
   type(error_data), allocatable, intent(out) :: error

   !> Name of the missing file
   character(len=*), intent(in) :: message

   allocate(error)
   error%message = message

end subroutine fatal_error


!> Error created when a file is missing or not found
subroutine file_not_found_error(error, file_name)

   !> Instance of the error data
   type(error_data), allocatable, intent(out) :: error

   !> Name of the missing file
   character(len=*), intent(in) :: file_name

   character(len=:), allocatable :: message

   message = "'"//file_name//"' could not be found, check if the file exists"

   call move_alloc(message, error%message)

end subroutine file_not_found_error


end module pkg_error
