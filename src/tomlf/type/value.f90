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

!> Class definitions for basic data types used for handling TOML
module tomlf_type_value
   use tomlf_constants, only : tfc, TOML_BAREKEY
   use tomlf_utils, only : toml_escape_string
   implicit none
   private

   public :: toml_value, toml_visitor, toml_key


   !> Abstract base value for TOML data types
   type, abstract :: toml_value

      !> Raw representation of the key to the TOML value
      character(kind=tfc, len=:), allocatable :: key

      !> Original source of the value
      integer :: origin = 0

   contains

      !> Accept a visitor to transverse the data structure
      procedure :: accept

      !> Get escaped key to TOML value
      procedure :: get_key

      !> Compare raw key of TOML value to input key
      procedure :: match_key

      !> Release allocation hold by TOML value
      procedure(destroy), deferred :: destroy

   end type toml_value


   !> Abstract visitor for TOML values
   type, abstract :: toml_visitor
   contains

      !> Visitor visiting a TOML value
      procedure(visit), deferred :: visit

   end type toml_visitor


   !> Thin wrapper around the deferred-size character intrinisc
   type :: toml_key

      !> Raw representation of the key to the TOML value
      character(kind=tfc, len=:), allocatable :: key

      !> Original source of the value
      integer :: origin = 0

   end type toml_key


   abstract interface
      !> Accept a visitor to transverse the data structure
      recursive subroutine visit(self, val)
         import toml_value, toml_visitor

         !> Instance of the visitor
         class(toml_visitor), intent(inout) :: self

         !> Value to visit
         class(toml_value), intent(inout) :: val
      end subroutine visit

      !> Deconstructor to cleanup allocations (optional)
      subroutine destroy(self)
         import toml_value

         !> Instance of the TOML value
         class(toml_value), intent(inout) :: self

      end subroutine destroy

   end interface


contains


!> Accept a visitor to transverse the data structure
recursive subroutine accept(self, visitor)

   !> Instance of the TOML value
   class(toml_value), intent(inout) :: self

   !> Visitor for this value
   class(toml_visitor), intent(inout) :: visitor

   call visitor%visit(self)

end subroutine accept


!> Get escaped key to TOML value
subroutine get_key(self, key)

   !> TOML value instance.
   class(toml_value), intent(in) :: self

   !> Contains valid TOML key on exit
   character(kind=tfc, len=:), allocatable :: key

   if (allocated(self%key)) then
      if (verify(self%key, TOML_BAREKEY) == 0 .and. len(self%key) > 0) then
         key = self%key
      else
         call toml_escape_string(self%key, key)
      end if
   end if

end subroutine get_key


!> Compare raw key of TOML value to input key
pure function match_key(self, key) result(match)

   !> TOML value instance.
   class(toml_value), intent(in) :: self

   !> TOML raw key to compare to
   character(kind=tfc, len=*), intent(in) :: key

   logical :: match

   if (allocated(self%key)) then
      match = key == self%key
   else
      match = .false.
   end if

end function match_key


end module tomlf_type_value
