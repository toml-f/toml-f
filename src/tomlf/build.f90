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

!> Functions to build a TOML data structures
!>
!> The build module defines a high level interface to work with TOML data structures
!> and construct them in a convenient way.
module tomlf_build
   use tomlf_build_array, only : get_value, set_value
   use tomlf_build_keyval, only : get_value, set_value
   use tomlf_build_merge, only : merge_table, merge_array
   use tomlf_build_table, only : get_value, set_value
   implicit none
   private

   public :: get_value, set_value, merge_table, merge_array


end module tomlf_build
