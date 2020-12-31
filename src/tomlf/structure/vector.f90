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
module tomlf_structure_vector
   use tomlf_constants, only : tfc
   use tomlf_structure_base, only : toml_ordered
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_vector, new_vector


   !> Wrapped TOML value to generate pointer list
   type :: toml_node

      !> TOML value payload
      class(toml_value), allocatable :: val

   end type toml_node


   !> Stores TOML values in a list of pointers
   type, extends(toml_ordered) :: toml_vector

      !> Current number of stored TOML values
      integer :: n = 0

      !> List of TOML values
      type(toml_node), allocatable :: lst(:)

   contains

      !> Get number of TOML values in the structure
      procedure :: get_len

      !> Find a TOML value based on its key
      procedure :: find

      !> Get TOML value at a given index
      procedure :: get

      !> Push back a TOML value to the structure
      procedure :: push_back

      !> Remove the first element from the structure
      procedure :: shift

      !> Remove the last element from the structure
      procedure :: pop

      !> Get list of all keys in the structure
      procedure :: get_keys

      !> Delete TOML value at a given key
      procedure :: delete

      !> Destroy the data structure
      procedure :: destroy

   end type toml_vector


   !> Initial storage capacity of the datastructure
   integer, parameter :: initial_size = 16


contains


!> Constructor for the storage data structure
subroutine new_vector(self, n)

   !> Instance of the structure
   type(toml_vector), intent(out) :: self

   !> Initial storage capacity
   integer, intent(in), optional :: n

   self%n = 0
   if (present(n)) then
      allocate(self%lst(min(1, n)))
   else
      allocate(self%lst(initial_size))
   end if

end subroutine new_vector


!> Get number of TOML values in the structure
pure function get_len(self) result(length)

   !> Instance of the structure
   class(toml_vector), intent(in), target :: self

   !> Current length of the ordered structure
   integer :: length

   length = self%n

end function get_len


!> Find a TOML value based on its key
subroutine find(self, key, ptr)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

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

end subroutine find


!> Get TOML value at a given index
subroutine get(self, idx, ptr)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

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
   class(toml_vector), intent(inout), target :: self

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
   class(toml_vector), intent(inout), target :: self

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
   class(toml_vector), intent(inout), target :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   if (self%n > 0) then
      call move_alloc(self%lst(self%n)%val, val)
      self%n = self%n - 1
   end if

end subroutine pop


!> Get list of all keys in the structure
subroutine get_keys(self, list)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   !> List of all keys
   type(toml_key), allocatable, intent(out) :: list(:)

   integer :: i

   allocate(list(self%n))

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         if (allocated(self%lst(i)%val%key)) then
            list(i)%key = self%lst(i)%val%key
         end if
      end if
   end do

end subroutine get_keys


!> Delete TOML value at a given key
subroutine delete(self, key)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

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
      call self%lst(idx)%val%destroy
      do i = idx+1, self%n
         call move_alloc(self%lst(i)%val, self%lst(i-1)%val)
      end do
      self%n = self%n - 1
   end if

end subroutine delete


!> Change size of the TOML value vector
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


!> Deconstructor for data structure
subroutine destroy(self)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   integer :: i

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         call self%lst(i)%val%destroy
      end if
   end do

   deallocate(self%lst)
   self%n = 0

end subroutine destroy


end module tomlf_structure_vector
