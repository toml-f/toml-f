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

!> Processing of the configuration file
!
!  Reexports the used parts of the TOML-Fortran API
module pkg_toml
   use pkg_error, only : error_data, fatal_error, file_not_found_error
   use tomlf, only : toml_table, toml_error, toml_parse, toml_stat, &
      & set_value, get_value
   implicit none
   private

   public :: read_config_file
   public :: toml_table, toml_stat, set_value, get_value


contains


!> Process the configuration file to a TOML data structure
subroutine read_config_file(table, config, error)

   !> TOML data structure
   type(toml_table), allocatable, intent(out) :: table

   !> Name of the package configuration file
   character(len=*), intent(in) :: config

   !> Error status of the operation
   type(error_data), allocatable, intent(out) :: error

   type(toml_error), allocatable :: parse_error
   integer :: unit
   logical :: exist

   inquire(file=config, exist=exist)

   if (.not.exist) then
      call file_not_found_error(error, config)
      return
   end if

   open(file=config, newunit=unit, status='old')
   call toml_parse(table, unit, parse_error)
   close(unit)

   if (allocated(parse_error)) then
      allocate(error)
      call move_alloc(parse_error%message, error%message)
      return
   end if

end subroutine read_config_file


end module pkg_toml
