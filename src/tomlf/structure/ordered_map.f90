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
module tomlf_structure_ordered_map
   use tomlf_constants, only : tfc
   use tomlf_structure_map, only : toml_map_structure
   use tomlf_structure_node, only : toml_node, resize
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_ordered_map, new_ordered_map


   !> Stores TOML values in a list of pointers
   type, extends(toml_map_structure) :: toml_ordered_map

      !> Current number of stored TOML values
      integer :: n = 0

      !> List of TOML values
      type(toml_node), allocatable :: lst(:)

   contains

      !> Get TOML value at a given key
      procedure :: get

      !> Push back a TOML value to the structure
      procedure :: push_back

      !> Remove TOML value at a given key and return it
      procedure :: pop

      !> Get list of all keys in the structure
      procedure :: get_keys

      !> Delete TOML value at a given key
      procedure :: delete

      !> Destroy the data structure
      procedure :: destroy

   end type toml_ordered_map


   !> Initial storage capacity of the datastructure
   integer, parameter :: initial_size = 16


contains


!> Constructor for the storage data structure
subroutine new_ordered_map(self, n)

   !> Instance of the structure
   type(toml_ordered_map), intent(out) :: self

   !> Initial storage capacity
   integer, intent(in), optional :: n

   self%n = 0
   if (present(n)) then
      allocate(self%lst(min(1, n)))
   else
      allocate(self%lst(initial_size))
   end if

end subroutine new_ordered_map


!> Get TOML value at a given key
subroutine get(self, key, ptr)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the stored value at given key
   class(toml_value), pointer, intent(out) :: ptr

   integer :: i

   nullify(ptr)

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         if (self%lst(i)%val%match_key(key)) then
            ptr => self%lst(i)%val
            exit
         end if
      end if
   end do

end subroutine get


!> Push back a TOML value to the structure
subroutine push_back(self, val)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

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


!> Get list of all keys in the structure
subroutine get_keys(self, list)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   !> List of all keys
   type(toml_key), allocatable, intent(out) :: list(:)

   integer :: i

   allocate(list(self%n))

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         if (allocated(self%lst(i)%val%key)) then
            list(i)%key = self%lst(i)%val%key
            list(i)%origin = self%lst(i)%val%origin
         end if
      end if
   end do

end subroutine get_keys


!> Remove TOML value at a given key and return it
subroutine pop(self, key, val)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> Removed TOML value
   class(toml_value), allocatable, intent(out) :: val

   integer :: idx, i

   idx = 0
   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         if (self%lst(i)%val%match_key(key)) then
            idx = i
            exit
         end if
      end if
   end do

   if (idx > 0) then
      call move_alloc(self%lst(idx)%val, val)
      do i = idx+1, self%n
         call move_alloc(self%lst(i)%val, self%lst(i-1)%val)
      end do
      self%n = self%n - 1
   end if

end subroutine pop


!> Delete TOML value at a given key
subroutine delete(self, key)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   class(toml_value), allocatable :: val

   call self%pop(key, val)
   if (allocated(val)) then
      call val%destroy()
   end if

end subroutine delete


!> Deconstructor for data structure
subroutine destroy(self)

   !> Instance of the structure
   class(toml_ordered_map), intent(inout), target :: self

   integer :: i

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         call self%lst(i)%val%destroy
      end if
   end do

   deallocate(self%lst)
   self%n = 0

end subroutine destroy


end module tomlf_structure_ordered_map
