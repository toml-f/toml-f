! This file is part of toml-f.
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

!> Version information on TOML-Fortran
module tomlf_version
   implicit none
   private

   public :: tomlf_version_string, tomlf_version_compact


   !> String representation of the TOML-Fortran version
   character(len=*), parameter :: tomlf_version_string = "0.2.0"

   !> Major version number of the above TOML-Fortran version
   integer, parameter :: major = 0

   !> Minor version number of the above TOML-Fortran version
   integer, parameter :: minor = 2

   !> Patch version number of the above TOML-Fortran version
   integer, parameter :: patch = 0

   !> Compact numeric representation of the TOML-Fortran version
   integer, parameter :: tomlf_version_compact = major*10000 + minor*100 + patch


end module tomlf_version
