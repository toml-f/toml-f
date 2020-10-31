! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Copyright (C) 2019-2020 Sebastian Ehlert
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

!> Implementation to ensure compatibility with older versions of the packaging
!  configuration format
module pkg_compatibility
   use pkg_error, only : error_data, fatal_error, file_not_found_error
   use pkg_package, only : package_data, new_package_data
   use tomlf, only : toml_table, toml_stat, get_value
   use tomlf_type, only : toml_value
   implicit none
   private

   public :: convert, pkg_format_version


   !> Current package format version
   integer, parameter :: pkg_format_version = 2


contains


!> Attempt to convert package meta data to a newer version
subroutine convert(table, format_version, error)

   !> TOML data structure representing the package meta data
   type(toml_table), intent(inout) :: table

   !> Current version of the format
   integer, intent(inout) :: format_version

   !> Error status of the operation
   type(error_data), allocatable, intent(out) :: error

   type(toml_table), pointer :: child
   type(toml_table), allocatable :: tmp
   class(toml_value), allocatable :: node
   integer :: stat

   if (format_version == 1) then
      ! `requirements` has been renamed to `dependencies`,
      ! the format inside the table remains the same
      call get_value(table, "requirements", child, requested=.false.)
      if (associated(child)) then
         ! deepcopy the complete dependencies table
         tmp = child
         tmp%key = "dependencies"
         ! delete the old entry
         call table%delete("requirements")
         ! move the allocation of the adjusted copy into the table:w
         call move_alloc(tmp, node)
         call table%push_back(node, stat)
         if (stat /= toml_stat%success) then
            call fatal_error(error, "Conversion failed, could migrate requirements table to dependencies table")
            return
         end if
      end if

      format_version = format_version + 1
   end if

end subroutine convert


end module pkg_compatibility
