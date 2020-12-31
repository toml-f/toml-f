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

!> Implementation of dependencies to other packages
module pkg_dependency
   use pkg_error, only : error_data, fatal_error
   use tomlf
   implicit none
   private

   public :: dependency_data, new_dependency


   !> Dependency for a package
   type :: dependency_data

      !> Name of the dependency
      character(len=:), allocatable :: name

      !> Version constraints
      character(len=:), allocatable :: version

      !> Optional variant information in case multiple vendors are available or
      !  multiple versions of the same dependency are provided
      character(len=:), allocatable :: variant

      !> Optional git URL to and build the requested dependency
      character(len=:), allocatable :: git

   contains

      !> Produce informative printout
      procedure :: info

   end type dependency_data


contains


!> Create a new dependency from a TOML data structure
subroutine new_dependency(self, table, error)

   !> Instance of the dependency data
   type(dependency_data), intent(out) :: self

   !> TOML data structure, might get modified in the process
   type(toml_table), intent(inout) :: table

   !> Error status of the operation
   type(error_data), allocatable, intent(out) :: error

   call table%get_key(self%name)

   call get_value(table, "version", self%version, "*")
   call get_value(table, "variant", self%variant)
   call get_value(table, "git", self%git)

   if (allocated(self%variant) .and. allocated(self%git)) then
      call fatal_error(error, "Cannot have both variant and git field for a dependency")
   end if

end subroutine new_dependency


!> Write information on the package data
subroutine info(self, unit)

   !> Instance of the package data
   class(dependency_data), intent(in) :: self

   !> Unit for IO
   integer, intent(in) :: unit

   write(unit, '(" - ", a, t30, a)') "Dependency name", self%name
   if (self%version /= "*") then
      write(unit, '(3x, a, t30, a)') "Required version", self%version
   end if

   if (allocated(self%git)) then
      write(unit, '(3x, a, t30, a)') "Git URL", self%git
   end if

   if (allocated(self%variant)) then
      write(unit, '(3x, a, t30, a)') "Variant used", self%variant
   end if

end subroutine info


end module pkg_dependency
