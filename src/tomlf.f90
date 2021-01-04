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

!> Minimal public API for TOML-Fortran
module tomlf
   use tomlf_build, only : get_value, set_value
   use tomlf_de, only : toml_parse
   use tomlf_error, only : toml_error, toml_stat
   use tomlf_ser, only : toml_serializer
   use tomlf_type, only : toml_table, toml_array, toml_key, is_array_of_tables, &
      & new_table, add_table, add_array, len
   use tomlf_utils_sort, only : sort
   use tomlf_version, only : tomlf_version_string, tomlf_version_compact, &
      & get_tomlf_version
   implicit none
   public

end module tomlf
