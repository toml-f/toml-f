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
   use tomlf_structure_base, only : toml_structure, toml_ordered
   use tomlf_structure_vector, only : toml_vector, new_vector
   implicit none
   private

   public :: toml_structure, toml_ordered
   public :: new_structure, new_ordered
   public :: len


   !> Overload len function
   interface len
      module procedure :: get_len
   end interface


contains


!> Constructor for the storage data structure
subroutine new_structure(self)

   !> Instance of the structure
   class(toml_structure), allocatable, intent(out) :: self

   type(toml_vector), allocatable :: vect

   allocate(vect)
   call new_vector(vect)
   call move_alloc(vect, self)

end subroutine new_structure


!> Constructor for the ordered storage data structure
subroutine new_ordered(self)

   !> Instance of the structure
   class(toml_ordered), allocatable, intent(out) :: self

   type(toml_vector), allocatable :: vect

   allocate(vect)
   call new_vector(vect)
   call move_alloc(vect, self)

end subroutine new_ordered


!> Get number of TOML values in the structure
pure function get_len(self) result(length)

   !> Instance of the structure
   class(toml_ordered), intent(in) :: self

   !> Current length of the ordered structure
   integer :: length

   length = self%get_len()

end function get_len


end module tomlf_structure
