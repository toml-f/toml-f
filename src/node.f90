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

!> Implementation of a basic storage structure as pointer list of pointers.
!>
!> This implementation does purposely not use pointer attributes in the
!> datastructure to make it safer to work with.
module tomlf_structure_node
   use tomlf_type_value, only : toml_value
   implicit none
   private

   public :: toml_node, resize


   !> Wrapped TOML value to generate pointer list
   type :: toml_node

      !> TOML value payload
      class(toml_value), allocatable :: val

   end type toml_node


   !> Initial storage capacity of the datastructure
   integer, parameter :: initial_size = 16


contains


!> Change size of the TOML value list
subroutine resize(list, n)

   !> Array of TOML values to be resized
   type(toml_node), allocatable, intent(inout), target :: list(:)

   !> New size of the list
   integer, intent(in) :: n

   type(toml_node), allocatable, target :: tmp(:)
   integer :: i


   if (allocated(list)) then
      call move_alloc(list, tmp)
      allocate(list(n))

      do i = 1, min(size(tmp), n)
         if (allocated(tmp(i)%val)) then
            call move_alloc(tmp(i)%val, list(i)%val)
         end if
      end do

      do i = n+1, size(tmp)
         if (allocated(tmp(i)%val)) then
            call tmp(i)%val%destroy
            deallocate(tmp(i)%val)
         end if
      end do

      deallocate(tmp)
   else
      allocate(list(n))
   end if

end subroutine resize

end module tomlf_structure_node
