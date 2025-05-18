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

!> Implementation of the TOML array data type.
module tomlf_type_array
   use tomlf_error, only : toml_stat
   use tomlf_type_value, only : toml_value, toml_visitor
   use tomlf_structure, only : toml_list_structure, new_list_structure
   implicit none
   private

   public :: toml_array, new_array, new, initialized, len


   !> TOML array
   type, extends(toml_value) :: toml_array

      !> Is an inline array rather than an array of tables
      logical :: inline = .true.

      !> Storage unit for TOML values of this array
      class(toml_list_structure), allocatable, private :: list

   contains

      !> Get the TOML value at a given index
      procedure :: get

      !> Append value to array
      procedure :: push_back

      !> Remove the first element from the array
      procedure :: shift

      !> Remove the last element from the array
      procedure :: pop

      !> Release allocation hold by TOML array
      procedure :: destroy

   end type toml_array


   !> Create standard constructor
   interface toml_array
      module procedure :: new_array_func
   end interface toml_array


   !> Overloaded constructor for TOML values
   interface new
      module procedure :: new_array
   end interface


   !> Overload len function
   interface len
      module procedure :: get_len
   end interface


   !> Check whether data structure is initialized properly
   interface initialized
      module procedure :: array_initialized
   end interface initialized


contains


!> Constructor to create a new TOML array and allocate the internal storage
subroutine new_array(self)

   !> Instance of the TOML array
   type(toml_array), intent(out) :: self

   call new_list_structure(self%list)

end subroutine new_array


!> Default constructor for TOML array type
function new_array_func() result(self)

   !> Instance of the TOML array
   type(toml_array) :: self

   call new_array(self)

end function new_array_func


!> Check whether data structure is initialized properly
pure function array_initialized(self) result(okay)

   !> Instance of the TOML array
   type(toml_array), intent(in) :: self

   !> Data structure is initialized
   logical :: okay

   okay = allocated(self%list)
end function array_initialized


!> Get number of TOML values in the array
pure function get_len(self) result(length)

   !> Instance of the TOML array
   class(toml_array), intent(in) :: self

   !> Current length of the array
   integer :: length

   length = self%list%get_len()

end function get_len


!> Get the TOML value at the respective index
subroutine get(self, idx, ptr)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> Index to the TOML value
   integer, intent(in) :: idx

   !> Pointer to the TOML value
   class(toml_value), pointer, intent(out) :: ptr

   call self%list%get(idx, ptr)

end subroutine get


!> Push back a TOML value to the array
subroutine push_back(self, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> TOML value to append to array
   class(toml_value), allocatable, intent(inout) :: val

   !> Status of operation
   integer, intent(out) :: stat

   if (allocated(val%key)) then
      stat = toml_stat%fatal
      return
   end if

   call self%list%push_back(val)

   stat = toml_stat%success

end subroutine push_back


!> Remove the first element from the data structure
subroutine shift(self, val)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   call self%list%shift(val)

end subroutine shift


!> Remove the last element from the data structure
subroutine pop(self, val)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   call self%list%pop(val)

end subroutine pop


!> Deconstructor to cleanup allocations (optional)
subroutine destroy(self)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%list)) then
      call self%list%destroy
      deallocate(self%list)
   end if

end subroutine destroy


end module tomlf_type_array
