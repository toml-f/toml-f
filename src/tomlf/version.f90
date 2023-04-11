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

!> Version information on TOML-Fortran
module tomlf_version
   implicit none
   private

   public :: get_tomlf_version
   public :: tomlf_version_string, tomlf_version_compact


   !> String representation of the TOML-Fortran version
   character(len=*), parameter :: tomlf_version_string = "0.4.1"

   !> Major version number of the above TOML-Fortran version
   integer, parameter :: tomlf_major = 0

   !> Minor version number of the above TOML-Fortran version
   integer, parameter :: tomlf_minor = 4

   !> Patch version number of the above TOML-Fortran version
   integer, parameter :: tomlf_patch = 1

   !> Compact numeric representation of the TOML-Fortran version
   integer, parameter :: tomlf_version_compact = &
      & tomlf_major*10000 + tomlf_minor*100 + tomlf_patch


contains


!> Getter function to retrieve TOML-Fortran version
subroutine get_tomlf_version(major, minor, patch, string)

   !> Major version number of the TOML-Fortran version
   integer, intent(out), optional :: major

   !> Minor version number of the TOML-Fortran version
   integer, intent(out), optional :: minor

   !> Patch version number of the TOML-Fortran version
   integer, intent(out), optional :: patch

   !> String representation of the TOML-Fortran version
   character(len=:), allocatable, intent(out), optional :: string

   if (present(major)) then
      major = tomlf_major
   end if
   if (present(minor)) then
      minor = tomlf_minor
   end if
   if (present(patch)) then
      patch = tomlf_patch
   end if
   if (present(string)) then
      string = tomlf_version_string
   end if

end subroutine get_tomlf_version


end module tomlf_version
