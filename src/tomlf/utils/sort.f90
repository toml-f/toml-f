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

!> Sorting algorithms to work with hash maps
module tomlf_utils_sort
   use tomlf_type_value, only : toml_key
   implicit none
   private

   public :: sort, compare_less


   !> Create overloaded interface for export
   interface sort
      module procedure :: sort_keys
   end interface


   abstract interface
      !> Define order relation between two TOML keys
      pure function compare_less(lhs, rhs) result(less)
         import :: toml_key
         !> Left hand side TOML key in comparison
         type(toml_key), intent (in) :: lhs
         !> Right hand side TOML key in comparison
         type(toml_key), intent (in) :: rhs
         !> Comparison result
         logical :: less
      end function compare_less
   end interface


contains


   !> Entry point for sorting algorithm
   pure subroutine sort_keys(list, idx, compare)

      !> List of TOML keys to be sorted
      type(toml_key), intent(inout) :: list(:)

      !> Optionally, mapping from unsorted list to sorted list
      integer, intent(out), optional :: idx(:)

      !> Function implementing the order relation between two TOML keys
      procedure(compare_less), optional :: compare

      integer  :: low, high, i
      type(toml_key), allocatable  :: sorted(:)
      integer, allocatable :: indexarray(:)

      low = 1
      high = size(list)

      allocate(sorted, source=list)

      allocate(indexarray(high), source=[(i, i=low, high)])

      if (present(compare)) then
         call quicksort(sorted, indexarray, low, high, compare)
      else
         call quicksort(sorted, indexarray, low, high, compare_keys_less)
      end if

      do i = low, high
         list(i) = sorted(indexarray(i))
      end do

      if (present(idx)) then
         idx = indexarray
      end if

   end subroutine sort_keys


   !> Actual quick sort implementation
   pure recursive subroutine quicksort(list, idx, low, high, less)
      type(toml_key), intent(inout) :: list(:)
      integer, intent(inout) :: idx(:)
      integer, intent(in) :: low, high
      procedure(compare_less) :: less

      integer :: i, last
      integer :: pivot

      if (low < high) then

         call swap(idx(low), idx((low + high)/2))
         last = low
         do i = low + 1, high
            if (less(list(idx(i)), list(idx(low)))) then
               last = last + 1
               call swap(idx(last), idx(i))
            end if
         end do
         call swap(idx(low), idx(last))
         pivot = last

         call quicksort(list, idx, low, pivot - 1, less)
         call quicksort(list, idx, pivot + 1, high, less)
      end if

   end subroutine quicksort


   !> Swap two integer values
   pure subroutine swap(lhs, rhs)
      integer, intent(inout) :: lhs
      integer, intent(inout) :: rhs

      integer :: tmp

      tmp = lhs
      lhs = rhs
      rhs = tmp

   end subroutine swap


   !> Default comparison between two TOML keys
   pure function compare_keys_less(lhs, rhs) result(less)
      type(toml_key), intent (in) :: lhs
      type(toml_key), intent (in) :: rhs
      logical :: less

      less = lhs%key < rhs%key

   end function compare_keys_less


end module tomlf_utils_sort
