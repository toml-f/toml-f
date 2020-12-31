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

!> Implementation of a package source target
module pkg_source
   use pkg_error, only : error_data, fatal_error
   use tomlf
   implicit none
   private

   public :: source_data, new_source


   !> Package source data, knows how to retrieve and verify itself
   type :: source_data

      !> URL to the package source
      character(len=:), allocatable :: url

      !> Path to version control repository
      character(len=:), allocatable :: git

      !> SHA-256 hash to verify artifacts downloaded from URL entry
      character(len=:), allocatable :: sha256

      !> Relative path in the build directory
      character(len=:), allocatable :: folder

   contains

      !> Produce informative printout
      procedure :: info

   end type source_data


contains


!> Create new source data from a TOML data structure
subroutine new_source(self, table, error)

   !> Instance of the dependency data
   type(source_data), intent(out) :: self

   !> TOML data structure, might get modified in the process
   type(toml_table), intent(inout) :: table

   !> Error status of the operation
   type(error_data), allocatable, intent(out) :: error

   call get_value(table, "url", self%url)
   call get_value(table, "sha256", self%sha256)

   call get_value(table, "git", self%git)

   if (.not.(allocated(self%git).or.allocated(self%url))) then
      call fatal_error(error, "Needs at least one of url or git methods to retrieve source targets")
      return
   end if

   if (allocated(self%git) .and. allocated(self%url)) then
      call fatal_error(error, "Cannot have both url and git field present for source target")
      return
   end if

   if (.not.allocated(self%sha256) .and. allocated(self%url)) then
      call fatal_error(error, "SHA256 checksum is required for URL source targets")
      return
   end if

   if (allocated(self%git) .and. allocated(self%sha256)) then
      call fatal_error(error, "SHA256 hash cannot be used with git sources")
      return
   end if

   call get_value(table, "folder", self%folder, ".")

end subroutine new_source


!> Write information on the package data
subroutine info(self, unit)

   !> Instance of the package data
   class(source_data), intent(in) :: self

   !> Unit for IO
   integer, intent(in) :: unit

   if (allocated(self%git)) then
      write(unit, '(" - ", a, t30, a)') "Git URL", self%git
   end if

   if (allocated(self%url)) then
      write(unit, '(" - ", a, t30, a)') "Source URL", self%url
   end if

   if (self%folder /= ".") then
      write(unit, '(3x, a, t30, a)') "Relative path", self%folder
   end if

end subroutine info


end module pkg_source
