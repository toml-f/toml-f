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
module tomlf_structure_array_list
   use tomlf_constants, only : tfc
   use tomlf_structure_list, only : toml_list_structure
   use tomlf_structure_node, only : toml_node, resize
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_array_list, new_array_list


   !> Stores TOML values in a list of pointers
   type, extends(toml_list_structure) :: toml_array_list

      !> Current number of stored TOML values
      integer :: n = 0

      !> List of TOML values
      type(toml_node), allocatable :: lst(:)

   contains

      !> Get number of TOML values in the structure
      procedure :: get_len

      !> Get TOML value at a given index
      procedure :: get

      !> Push back a TOML value to the structure
      procedure :: push_back

      !> Remove the first element from the structure
      procedure :: shift

      !> Remove the last element from the structure
      procedure :: pop

      !> Destroy the data structure
      procedure :: destroy

   end type toml_array_list


   !> Initial storage capacity of the datastructure
   integer, parameter :: initial_size = 16


contains


!> Constructor for the storage data structure
subroutine new_array_list(self, n)

   !> Instance of the structure
   type(toml_array_list), intent(out) :: self

   !> Initial storage capacity
   integer, intent(in), optional :: n

   self%n = 0
   if (present(n)) then
      allocate(self%lst(min(1, n)))
   else
      allocate(self%lst(initial_size))
   end if

end subroutine new_array_list


!> Get number of TOML values in the structure
pure function get_len(self) result(length)

   !> Instance of the structure
   class(toml_array_list), intent(in), target :: self

   !> Current length of the ordered structure
   integer :: length

   length = self%n

end function get_len


!> Get TOML value at a given index
subroutine get(self, idx, ptr)

   !> Instance of the structure
   class(toml_array_list), intent(inout), target :: self

   !> Position in the ordered structure
   integer, intent(in) :: idx

   !> Pointer to the stored value at given index
   class(toml_value), pointer, intent(out) :: ptr

   nullify(ptr)

   if (idx > 0 .and. idx <= self%n) then
      if (allocated(self%lst(idx)%val)) then
         ptr => self%lst(idx)%val
      end if
   end if

end subroutine get


!> Push back a TOML value to the structure
subroutine push_back(self, val)

   !> Instance of the structure
   class(toml_array_list), intent(inout), target :: self

   !> TOML value to be stored
   class(toml_value), allocatable, intent(inout) :: val

   integer :: m

   if (.not.allocated(self%lst)) then
      call resize(self%lst, initial_size)
   end if

   m = size(self%lst)
   if (self%n >= m) then
      call resize(self%lst, m + m/2 + 1)
   end if

   self%n = self%n + 1
   call move_alloc(val, self%lst(self%n)%val)

end subroutine push_back


!> Remove the first element from the data structure
subroutine shift(self, val)

   !> Instance of the structure
   class(toml_array_list), intent(inout), target :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   integer :: i

   if (self%n > 0) then
      call move_alloc(self%lst(1)%val, val)
      do i = 2, self%n
         call move_alloc(self%lst(i)%val, self%lst(i-1)%val)
      end do
      self%n = self%n - 1
   end if

end subroutine shift


!> Remove the last element from the data structure
subroutine pop(self, val)

   !> Instance of the structure
   class(toml_array_list), intent(inout), target :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   if (self%n > 0) then
      call move_alloc(self%lst(self%n)%val, val)
      self%n = self%n - 1
   end if

end subroutine pop


!> Deconstructor for data structure
subroutine destroy(self)

   !> Instance of the structure
   class(toml_array_list), intent(inout), target :: self

   integer :: i

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         call self%lst(i)%val%destroy
      end if
   end do

   deallocate(self%lst)
   self%n = 0

end subroutine destroy


end module tomlf_structure_array_list
