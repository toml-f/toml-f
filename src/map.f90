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

!> Abstract base class definitions for data structures to store TOML values
module tomlf_structure_map
   use tomlf_constants, only : tfc
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_map_structure


   !> Abstract data structure
   type, abstract :: toml_map_structure
   contains

      !> Get TOML value at a given key
      procedure(get), deferred :: get

      !> Push back a TOML value to the structure
      procedure(push_back), deferred :: push_back

      !> Get list of all keys in the structure
      procedure(get_keys), deferred :: get_keys

      !> Remove TOML value at a given key and return it
      procedure(pop), deferred :: pop

      !> Delete TOML value at a given key
      procedure(delete), deferred :: delete

      !> Destroy the data structure
      procedure(destroy), deferred :: destroy

   end type toml_map_structure


   abstract interface
      !> Get TOML value at a given key
      subroutine get(self, key, ptr)
         import :: toml_map_structure, toml_value, tfc

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

         !> Key to the TOML value
         character(kind=tfc, len=*), intent(in) :: key

         !> Pointer to the stored value at given key
         class(toml_value), pointer, intent(out) :: ptr
      end subroutine get


      !> Push back a TOML value to the structure
      subroutine push_back(self, val)
         import :: toml_map_structure, toml_value

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

         !> TOML value to be stored
         class(toml_value), allocatable, intent(inout) :: val

      end subroutine push_back


      !> Get list of all keys in the structure
      subroutine get_keys(self, list)
         import :: toml_map_structure, toml_key

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

         !> List of all keys
         type(toml_key), allocatable, intent(out) :: list(:)

      end subroutine get_keys


      !> Remove TOML value at a given key and return it
      subroutine pop(self, key, val)
         import :: toml_map_structure, toml_value, tfc

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

         !> Key to the TOML value
         character(kind=tfc, len=*), intent(in) :: key

         !> Removed TOML value
         class(toml_value), allocatable, intent(out) :: val

      end subroutine pop


      !> Delete TOML value at a given key
      subroutine delete(self, key)
         import :: toml_map_structure, toml_value, tfc

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

         !> Key to the TOML value
         character(kind=tfc, len=*), intent(in) :: key

      end subroutine delete


      !> Deconstructor for data structure
      subroutine destroy(self)
         import :: toml_map_structure

         !> Instance of the structure
         class(toml_map_structure), intent(inout), target :: self

      end subroutine destroy

   end interface


end module tomlf_structure_map
