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
module tomlf_structure_list
   use tomlf_constants, only : tfc
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_list_structure


   !> Ordered data structure, allows iterations
   type, abstract :: toml_list_structure
   contains

      !> Get number of TOML values in the structure
      procedure(get_len), deferred :: get_len

      !> Push back a TOML value to the structure
      procedure(push_back), deferred :: push_back

      !> Remove the first element from the structure
      procedure(shift), deferred :: shift

      !> Remove the last element from the structure
      procedure(pop), deferred :: pop

      !> Get TOML value at a given index
      procedure(get), deferred :: get

      !> Destroy the data structure
      procedure(destroy), deferred :: destroy

   end type toml_list_structure


   abstract interface
      !> Get number of TOML values in the structure
      pure function get_len(self) result(length)
         import :: toml_list_structure

         !> Instance of the structure
         class(toml_list_structure), intent(in), target :: self

         !> Current length of the ordered structure
         integer :: length
      end function get_len


      !> Get TOML value at a given index
      subroutine get(self, idx, ptr)
         import :: toml_list_structure, toml_value

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

         !> Position in the ordered structure
         integer, intent(in) :: idx

         !> Pointer to the stored value at given index
         class(toml_value), pointer, intent(out) :: ptr
      end subroutine get


      !> Push back a TOML value to the structure
      subroutine push_back(self, val)
         import :: toml_list_structure, toml_value

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

         !> TOML value to be stored
         class(toml_value), allocatable, intent(inout) :: val

      end subroutine push_back


      !> Remove the first element from the data structure
      subroutine shift(self, val)
         import :: toml_list_structure, toml_value

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

         !> TOML value to be retrieved
         class(toml_value), allocatable, intent(out) :: val

      end subroutine shift


      !> Remove the last element from the data structure
      subroutine pop(self, val)
         import :: toml_list_structure, toml_value

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

         !> TOML value to be retrieved
         class(toml_value), allocatable, intent(out) :: val

      end subroutine pop


      !> Delete TOML value at a given key
      subroutine delete(self, key)
         import :: toml_list_structure, toml_value, tfc

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

         !> Key to the TOML value
         character(kind=tfc, len=*), intent(in) :: key

      end subroutine delete


      !> Deconstructor for data structure
      subroutine destroy(self)
         import :: toml_list_structure

         !> Instance of the structure
         class(toml_list_structure), intent(inout), target :: self

      end subroutine destroy

   end interface


end module tomlf_structure_list
