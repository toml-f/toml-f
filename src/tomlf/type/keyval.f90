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

!> TOML key-value pair
module tomlf_type_keyval
   use tomlf_constants, only : tfc
   use tomlf_type_value, only : toml_value, toml_visitor
   implicit none
   private

   public :: toml_keyval, new_keyval, new


   !> TOML key-value pair
   type, extends(toml_value) :: toml_keyval

      !> Raw content of the TOML value
      character(kind=tfc, len=:), allocatable :: raw

   contains

      !> Release allocation hold by TOML key-value pair
      procedure :: destroy

   end type toml_keyval


   !> Overloaded constructor for TOML values
   interface new
      module procedure :: new_keyval
   end interface


contains


!> Constructor to create a new TOML key-value pair
subroutine new_keyval(self)

   !> Instance of the TOML key-value pair
   type(toml_keyval), intent(out) :: self

   associate(self => self); end associate

end subroutine new_keyval


!> Deconstructor to cleanup allocations (optional)
subroutine destroy(self)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%raw)) then
      deallocate(self%raw)
   end if

end subroutine destroy


end module tomlf_type_keyval
