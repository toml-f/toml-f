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

!> Complete reexport of the public API of TOML-Fortran
module tomlf_all
   use tomlf_build
   use tomlf_constants
   use tomlf_de
   use tomlf_ser
   use tomlf_type
   use tomlf_version
   implicit none
   public

end module tomlf_all