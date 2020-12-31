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

!> Merge TOML data structures.
!>
!> Merge policy:
!> - copy key-value pair in case it is not present in table
!> - copy subtable in case it is not present in table
!> - copy array in case it is not present in table
!> - merge subtable in case it is present in table
!> - append array in case it is present in table
module tomlf_build_merge
   use tomlf_constants, only : tfc
   use tomlf_type, only : toml_table, toml_array, toml_keyval, toml_value, &
      & toml_key, len
   implicit none
   private

   public :: merge_table, merge_array


contains


!> Merge TOML tables by appending their values
recursive subroutine merge_table(lhs, rhs)

   !> Instance of table to merge into
   class(toml_table), intent(inout) :: lhs

   !> Instance of table to be merged
   class(toml_table), intent(inout) :: rhs

   type(toml_key), allocatable :: list(:)
   class(toml_value), pointer :: ptr1, ptr2
   class(toml_value), allocatable :: tmp
   logical :: has_key
   integer :: i, n, stat

   call rhs%get_keys(list)
   n = size(list, 1)

   do i = 1, n
      if (allocated(tmp)) deallocate(tmp)
      call rhs%get(list(i)%key, ptr1)
      has_key = lhs%has_key(list(i)%key)
      select type(ptr1)
      class is(toml_keyval)
         if (.not.has_key) then
            allocate(tmp, source=ptr1)
            call lhs%push_back(tmp, stat)
         end if
      class is(toml_array)
         if (has_key) then
            call lhs%get(list(i)%key, ptr2)
            select type(ptr2)
            class is(toml_array)
               call merge_array(ptr2, ptr1)
            end select
         else
            allocate(tmp, source=ptr1)
            call lhs%push_back(tmp, stat)
         end if
      class is(toml_table)
         if (has_key) then
            call lhs%get(list(i)%key, ptr2)
            select type(ptr2)
            class is(toml_table)
               call merge_table(ptr2, ptr1)
            end select
         else
            allocate(tmp, source=ptr1)
            call lhs%push_back(tmp, stat)
         end if
      end select
   end do

end subroutine merge_table


!> Append values from one TOML array to another
recursive subroutine merge_array(lhs, rhs)

   !> Instance of array to merge into
   class(toml_array), intent(inout) :: lhs

   !> Instance of array to be merged
   class(toml_array), intent(inout) :: rhs

   class(toml_value), pointer :: ptr
   class(toml_value), allocatable :: tmp
   integer :: n, i, stat

   n = len(rhs)

   do i = 1, n
      call rhs%get(i, ptr)
      if (allocated(tmp)) deallocate(tmp)
      allocate(tmp, source=ptr)
      call lhs%push_back(tmp, stat)
   end do

end subroutine merge_array


end module tomlf_build_merge
