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

!> Implementation of the TOML table data type.
!>
!> Every TOML document contains at least one (root) table which holds key-value
!> pairs, arrays and other tables.
module tomlf_type_table
   use tomlf_constants, only : tfc
   use tomlf_error, only : toml_stat
   use tomlf_type_value, only : toml_value, toml_visitor, toml_key
   use tomlf_structure, only : toml_map_structure, new_map_structure
   implicit none
   private

   public :: toml_table, new_table, new, initialized


   !> TOML table
   type, extends(toml_value) :: toml_table

      !> Table was implictly created
      logical :: implicit = .false.

      !> Is an inline table and is therefore non-extendable
      logical :: inline = .false.

      !> Storage unit for TOML values of this table
      class(toml_map_structure), allocatable, private :: map

   contains

      !> Get the TOML value associated with the respective key
      procedure :: get

      !> Get list of all keys in this table
      procedure :: get_keys

      !> Check if key is already present in this table instance
      procedure :: has_key

      !> Append value to table (checks automatically for key)
      procedure :: push_back

      !> Remove TOML value at a given key and return it
      procedure :: pop

      !> Delete TOML value at a given key
      procedure :: delete

      !> Release allocation hold by TOML table
      procedure :: destroy

   end type toml_table


   !> Create standard constructor
   interface toml_table
      module procedure :: new_table_func
   end interface toml_table


   !> Overloaded constructor for TOML values
   interface new
      module procedure :: new_table
   end interface


   !> Check whether data structure is initialized properly
   interface initialized
      module procedure :: table_initialized
   end interface initialized


contains


!> Constructor to create a new TOML table and allocate the internal storage
subroutine new_table(self)

   !> Instance of the TOML table
   type(toml_table), intent(out) :: self

   call new_map_structure(self%map)

end subroutine new_table


!> Default constructor for TOML table type
function new_table_func() result(self)

   !> Instance of the TOML table
   type(toml_table) :: self

   call new_table(self)

end function new_table_func


!> Check whether data structure is initialized properly
pure function table_initialized(self) result(okay)

   !> Instance of the TOML table
   type(toml_table), intent(in) :: self

   !> Data structure is initialized
   logical :: okay

   okay = allocated(self%map)
end function table_initialized


!> Get the TOML value associated with the respective key
subroutine get(self, key, ptr)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the TOML value
   class(toml_value), pointer, intent(out) :: ptr

   call self%map%get(key, ptr)

end subroutine get


!> Get list of all keys in this table
subroutine get_keys(self, list)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> List of all keys
   type(toml_key), allocatable, intent(out) :: list(:)

   call self%map%get_keys(list)

end subroutine get_keys


!> Check if a key is present in the table
function has_key(self, key) result(found)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> TOML value is present in table
   logical :: found

   class(toml_value), pointer :: ptr

   call self%map%get(key, ptr)

   found = associated(ptr)

end function has_key


!> Push back a TOML value to the table
subroutine push_back(self, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> TOML value to append to table
   class(toml_value), allocatable, intent(inout) :: val

   !> Status of operation
   integer, intent(out) :: stat

   class(toml_value), pointer :: ptr

   if (.not.allocated(val)) then
      stat = merge(self%origin, toml_stat%fatal, self%origin > 0)
      return
   end if

   if (.not.allocated(val%key)) then
      stat = merge(val%origin, toml_stat%fatal, val%origin > 0)
      return
   end if

   call self%get(val%key, ptr)
   if (associated(ptr)) then
      stat = merge(ptr%origin, toml_stat%duplicate_key, ptr%origin > 0)
      return
   end if

   call self%map%push_back(val)

   stat = toml_stat%success

end subroutine push_back


!> Remove TOML value at a given key and return it
subroutine pop(self, key, val)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> Removed TOML value to return
   class(toml_value), allocatable, intent(out) :: val

   call self%map%pop(key, val)

end subroutine pop


!> Delete TOML value at a given key
subroutine delete(self, key)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   call self%map%delete(key)

end subroutine delete


!> Deconstructor to cleanup allocations (optional)
subroutine destroy(self)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%map)) then
      call self%map%destroy
      deallocate(self%map)
   end if

end subroutine destroy


end module tomlf_type_table
