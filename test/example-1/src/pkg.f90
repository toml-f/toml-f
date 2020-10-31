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

!> Export of the public API
module pkg
   use pkg_compatibility, only : convert, pkg_format_version
   use pkg_error, only : error_data, fatal_error, file_not_found_error
   use pkg_package, only : package_data, new_package_data
   use pkg_toml, only : read_config_file, toml_table, get_value
   implicit none
   private

   public :: get_package_data
   public :: package_data, error_data


contains


!> Obtain package meta data from an configuation file
subroutine get_package_data(pkg_data, pkg_file, error)

   !> Parsed package meta data
   type(package_data), intent(out) :: pkg_data

   !> Name of the package configuration file
   character(len=*), intent(in) :: pkg_file

   !> Error status of the operation
   type(error_data), allocatable, intent(out) :: error

   type(toml_table), allocatable :: table
   integer :: format_version

   call read_config_file(table, pkg_file, error)
   if (allocated(error)) return

   if (.not.allocated(table)) then
      call fatal_error(error, "Unclassified error while reading: '"//pkg_file//"'")
      return
   end if

   call get_value(table, "pkg_version", format_version, pkg_format_version)
   if (format_version > pkg_format_version) then
      call fatal_error(error, "This packager does not support the provided package file format")
      return
   end if
   if (format_version < pkg_format_version) then
      call convert(table, format_version, error)
      if (allocated(error)) return
   end if

   call new_package_data(pkg_data, table, error)

end subroutine get_package_data


end module pkg
