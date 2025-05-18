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

!> Abstraction layer for the actual storage of the data structure.
!>
!> The structure implementations provide the actual storage for TOML values, with
!> a generic enough interface to make the definition of the TOML data structures
!> independent of the actual algorithm used for storing the TOML values.
!>
!> Every data structure defined here should strive to only use allocatable
!> data types and limit the use of pointer attributes as they interfer with
!> the automatic memory management of Fortran. A well defined data structure
!> in allocatables allows deep-copying of TOML values by assignment, data structures
!> requiring pointer attributes have to define an assignment(=) interface to
!> allow deep-copying of TOML values.
module tomlf_structure
   use tomlf_structure_list, only : toml_list_structure
   use tomlf_structure_map, only : toml_map_structure
   use tomlf_structure_array_list, only : toml_array_list, new_array_list
   use tomlf_structure_ordered_map, only : toml_ordered_map, new_ordered_map
   implicit none
   private

   public :: toml_list_structure, toml_map_structure
   public :: new_list_structure, new_map_structure


contains


!> Constructor for the ordered storage data structure
subroutine new_list_structure(self)

   !> Instance of the structure
   class(toml_list_structure), allocatable, intent(out) :: self

   block
      type(toml_array_list), allocatable :: list

      allocate(list)
      call new_array_list(list)
      call move_alloc(list, self)
   end block

end subroutine new_list_structure


!> Constructor for the storage data structure
subroutine new_map_structure(self)

   !> Instance of the structure
   class(toml_map_structure), allocatable, intent(out) :: self

   block
      type(toml_ordered_map), allocatable :: map

      allocate(map)
      call new_ordered_map(map)
      call move_alloc(map, self)
   end block

end subroutine new_map_structure


end module tomlf_structure
