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

!> Error handling for TOML Fortran
!>
!> This module provides the [[toml_error]] type for error reporting and
!> the [[toml_stat]] enumerator for status codes returned by various
!> TOML Fortran procedures.
!>
!> ## Error Handling
!>
!> Most parsing and access functions accept an optional `error` argument
!> of type [[toml_error]]. If an error occurs, this will be allocated
!> and contain a descriptive message:
!>
!>```fortran
!> type(toml_error), allocatable :: error
!> call toml_load(table, "config.toml", error=error)
!> if (allocated(error)) print '(a)', error%message
!>```
!>
!> ## Status Codes
!>
!> The [[toml_stat]] enumerator provides named constants for common
!> error conditions like `toml_stat%duplicate_key` or `toml_stat%type_mismatch`.
module tomlf_error
   use tomlf_constants, only : tfc, TOML_NEWLINE
   implicit none
   private

   public :: toml_stat, toml_error, make_error


   !> Possible TOML Fortran status codes
   type :: enum_stat

      !> Successful run
      integer :: success = 0

      !> Internal error:
      !>
      !> General undefined error state, usually caused by algorithmic errors.
      integer :: fatal = -1

      !> Duplicate key encountered
      integer :: duplicate_key = -2

      !> Incorrect type when reading a value
      integer :: type_mismatch = -3

      !> Conversion error when downcasting a value
      integer :: conversion_error = -4

      !> Key not present in table
      integer :: missing_key = -5

   end type enum_stat

   !> Actual enumerator for return states
   !>
   !> | Name | Description |
   !> |------|-------------|
   !> | `success` | Operation completed successfully |
   !> | `fatal` | Internal error or undefined error state |
   !> | `duplicate_key` | Duplicate key encountered in table |
   !> | `type_mismatch` | Incorrect type when reading a value |
   !> | `conversion_error` | Error when converting or downcasting a value |
   !> | `missing_key` | Requested key not present in table |
   type(enum_stat), parameter :: toml_stat = enum_stat()


   !> Error message produced by TOML-Fortran
   type :: toml_error

      !> Error code
      integer :: stat = toml_stat%fatal

      !> Payload of the error
      character(kind=tfc, len=:), allocatable :: message

   end type toml_error


contains

!> Create new error message
subroutine make_error(error, message, stat)
   !> Error report
   type(toml_error), allocatable, intent(out) :: error
   !> Message for the error
   character(*, tfc), intent(in) :: message
   !> Status code
   integer, intent(in), optional :: stat

   allocate(error)
   error%message = message
   if (present(stat)) then
      error%stat = stat
   else
      error%stat = toml_stat%fatal
   end if
end subroutine make_error

end module tomlf_error
