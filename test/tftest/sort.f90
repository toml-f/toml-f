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

module tftest_sort
   use testdrive
   use tomlf, only : toml_key, sort
   implicit none
   private

   public :: collect_sort


contains


!> Collect all exported unit tests
subroutine collect_sort(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("sorted", test_sorted), &
      & new_unittest("reversed", test_reversed), &
      & new_unittest("unsorted", test_unsorted) &
      & ]

end subroutine collect_sort


subroutine test_sorted(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_key), allocatable :: list(:)
   integer :: i
   character(len=*), parameter :: expected(10) = &
      & ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   list = [ &
      & toml_key("0"), &
      & toml_key("1"), &
      & toml_key("2"), &
      & toml_key("3"), &
      & toml_key("4"), &
      & toml_key("5"), &
      & toml_key("6"), &
      & toml_key("7"), &
      & toml_key("8"), &
      & toml_key("9")]

   call sort(list)

   do i = 1, size(list)
      if (list(i)%key /= expected(i)) then
         call test_failed(error, "List is not sorted correctly")
         exit
      end if
   end do
   if (allocated(error)) return

end subroutine test_sorted


subroutine test_reversed(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_key), allocatable :: list(:)
   integer :: i
   character(len=*), parameter :: expected(10) = &
      & ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   list = [ &
      & toml_key("9"), &
      & toml_key("8"), &
      & toml_key("7"), &
      & toml_key("6"), &
      & toml_key("5"), &
      & toml_key("4"), &
      & toml_key("3"), &
      & toml_key("2"), &
      & toml_key("1"), &
      & toml_key("0")]

   call sort(list)

   do i = 1, size(list)
      if (list(i)%key /= expected(i)) then
         call test_failed(error, "List is not sorted correctly")
         exit
      end if
   end do
   if (allocated(error)) return

end subroutine test_reversed


subroutine test_unsorted(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_key), allocatable :: list(:)
   integer :: i
   character(len=*), parameter :: expected(10) = &
      & ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   list = [ &
      & toml_key("7"), &
      & toml_key("9"), &
      & toml_key("6"), &
      & toml_key("8"), &
      & toml_key("5"), &
      & toml_key("2"), &
      & toml_key("4"), &
      & toml_key("1"), &
      & toml_key("3"), &
      & toml_key("0")]

   call sort(list)

   do i = 1, size(list)
      if (list(i)%key /= expected(i)) then
         call test_failed(error, "List is not sorted correctly")
         exit
      end if
   end do
   if (allocated(error)) return

end subroutine test_unsorted


end module tftest_sort
