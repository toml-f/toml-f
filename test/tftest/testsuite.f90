! This file is part of toml-f.
!
! Copyright (C) 2019-2020 Sebastian Ehlert
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

!> Define some procedures to automate collecting and launching of tests
module tftest_testsuite
   use tomlf_constants, only : tfc, tf_sp, tf_dp, tf_i1, tf_i2, tf_i4, tf_i8, &
      & TOML_NEWLINE
   use tomlf_error, only : toml_error, toml_stat
   implicit none
   private

   public :: run_testsuite, new_unittest, test_failed, check
   public :: toml_unittest, toml_error


   interface check
      module procedure :: check_stat
      module procedure :: check_float_sp
      module procedure :: check_float_dp
      module procedure :: check_int_i1
      module procedure :: check_int_i2
      module procedure :: check_int_i4
      module procedure :: check_int_i8
      module procedure :: check_bool
   end interface check



   abstract interface
      !> Entry point for tests
      subroutine test_interface(error)
         import :: toml_error

         !> Error handling
         type(toml_error), allocatable, intent(out) :: error

      end subroutine test_interface
   end interface


   !> Declaration of a unit test
   type :: toml_unittest

      !> Name of the test
      character(len=:), allocatable :: name

      !> Entry point of the test
      procedure(test_interface), pointer, nopass :: test => null()

      !> Whether test is supposed to fail
      logical :: should_fail = .false.

   end type toml_unittest


   abstract interface
      !> Collect all tests
      subroutine collect_interface(testsuite)
         import :: toml_unittest

         !> Collection of tests
         type(toml_unittest), allocatable, intent(out) :: testsuite(:)

      end subroutine collect_interface
   end interface


   integer, parameter :: buffersize = 128


contains


!> Driver for testsuite
subroutine run_testsuite(collect, unit, stat)

   !> Collect tests
   procedure(collect_interface) :: collect

   !> Unit for IO
   integer, intent(in) :: unit

   !> Number of failed tests
   integer, intent(out) :: stat

   type(toml_unittest), allocatable :: testsuite(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'
   character(len=*), parameter :: indent = repeat(" ", 5) // repeat(".", 3)
   type(toml_error), allocatable :: error
   integer :: ii

   stat = 0

   call collect(testsuite)

   do ii = 1, size(testsuite)
      write(unit, '("#", *(1x, a))') "Starting", testsuite(ii)%name, "..."
      call testsuite(ii)%test(error)
      if (allocated(error) .neqv. testsuite(ii)%should_fail) then
         if (testsuite(ii)%should_fail) then
            write(unit, fmt) indent, testsuite(ii)%name, "[UNEXPECTED PASS]"
         else
            write(unit, fmt) indent, testsuite(ii)%name, "[FAILED]"
         end if
         stat = stat + 1
      else
         if (testsuite(ii)%should_fail) then
            write(unit, fmt) indent, testsuite(ii)%name, "[EXPECTED FAIL]"
         else
            write(unit, fmt) indent, testsuite(ii)%name, "[PASSED]"
         end if
      end if
      if (allocated(error)) then
         write(unit, '(a)') error%message
      end if
   end do

end subroutine run_testsuite


!> Register a new unit test
function new_unittest(name, test, should_fail) result(self)

   !> Name of the test
   character(len=*), intent(in) :: name

   !> Entry point for the test
   procedure(test_interface) :: test

   !> Whether test is supposed to error or not
   logical, intent(in), optional :: should_fail

   !> Newly registered test
   type(toml_unittest) :: self

   self%name = name
   self%test => test
   if (present(should_fail)) self%should_fail = should_fail

end function new_unittest


!> Report a failing unittest
subroutine test_failed(error, message, more)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> A detailed message describing the error
   character(kind=tfc, len=*), intent(in) :: message

   !> Another line of error message
   character(kind=tfc, len=*), intent(in), optional :: more

   allocate(error)
   error%stat = toml_stat%fatal

   if (present(more)) then
      error%message = message // TOML_NEWLINE // more
   else
      error%message = message
   end if

end subroutine test_failed


subroutine check_stat(error, stat, message, more)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Status of operation
   integer, intent(in) :: stat

   !> A detailed message describing the error
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Another line of error message
   character(kind=tfc, len=*), intent(in), optional :: more

   if (stat /= toml_stat%success) then
      if (present(message)) then
         call test_failed(error, message, more)
      else
         call test_failed(error, "Non-zero exit code encountered", more)
      end if
   end if

end subroutine check_stat


subroutine check_float_dp(error, actual, expected, message, more, thr, rel)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Found floating point value
   real(tf_dp), intent(in) :: actual

   !> Expected floating point value
   real(tf_dp), intent(in) :: expected

   !> A detailed message describing the error
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Another line of error message
   character(kind=tfc, len=*), intent(in), optional :: more

   !> Allowed threshold for matching floating point values
   real(tf_dp), intent(in), optional :: thr

   !> Check for relative errors instead
   logical, intent(in), optional :: rel

   logical :: relative
   real(tf_dp) :: diff, threshold

   if (present(thr)) then
      threshold = thr
   else
      threshold = epsilon(expected)
   end if

   if (present(rel)) then
      relative = rel
   else
      relative = .false.
   end if

   if (relative) then
      diff = abs(actual - expected) / expected
   else
      diff = abs(actual - expected)
   end if

   if (diff > threshold) then
      if (present(message)) then
         call test_failed(error, message, more)
      else
         call test_failed(error, "Floating point value missmatch", more)
      end if
   end if

end subroutine check_float_dp


subroutine check_float_sp(error, actual, expected, message, more, thr, rel)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Found floating point value
   real(tf_sp), intent(in) :: actual

   !> Expected floating point value
   real(tf_sp), intent(in) :: expected

   !> A detailed message describing the error
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Another line of error message
   character(kind=tfc, len=*), intent(in), optional :: more

   !> Allowed threshold for matching floating point values
   real(tf_sp), intent(in), optional :: thr

   !> Check for relative errors instead
   logical, intent(in), optional :: rel

   logical :: relative
   real(tf_sp) :: diff, threshold

   if (present(thr)) then
      threshold = thr
   else
      threshold = epsilon(expected)
   end if

   if (present(rel)) then
      relative = rel
   else
      relative = .false.
   end if

   if (relative) then
      diff = abs(actual - expected) / expected
   else
      diff = abs(actual - expected)
   end if

   if (diff > threshold) then
      if (present(message)) then
         call test_failed(error, message, more)
      else
         call test_failed(error, "Floating point value missmatch", more)
      end if
   end if

end subroutine check_float_sp


subroutine check_int_i1(error, actual, expected, message, more)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Found integer value
   integer(tf_i1), intent(in) :: actual

   !> Expected integer value
   integer(tf_i1), intent(in) :: expected

   !> A detailed message describing the error
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Another line of error message
   character(kind=tfc, len=*), intent(in), optional :: more

   if (expected /= actual) then
      if (present(message)) then
         call test_failed(error, message, more)
      else
         call test_failed(error, "Integer value missmatch", more)
      end if
   end if

end subroutine check_int_i1


subroutine check_int_i2(error, actual, expected, message, more)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Found integer value
   integer(tf_i2), intent(in) :: actual

   !> Expected integer value
   integer(tf_i2), intent(in) :: expected

   !> A detailed message describing the error
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Another line of error message
   character(kind=tfc, len=*), intent(in), optional :: more

   if (expected /= actual) then
      if (present(message)) then
         call test_failed(error, message, more)
      else
         call test_failed(error, "Integer value missmatch", more)
      end if
   end if

end subroutine check_int_i2


subroutine check_int_i4(error, actual, expected, message, more)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Found integer value
   integer(tf_i4), intent(in) :: actual

   !> Expected integer value
   integer(tf_i4), intent(in) :: expected

   !> A detailed message describing the error
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Another line of error message
   character(kind=tfc, len=*), intent(in), optional :: more

   if (expected /= actual) then
      if (present(message)) then
         call test_failed(error, message, more)
      else
         call test_failed(error, "Integer value missmatch", more)
      end if
   end if

end subroutine check_int_i4


subroutine check_int_i8(error, actual, expected, message, more)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Found integer value
   integer(tf_i8), intent(in) :: actual

   !> Expected integer value
   integer(tf_i8), intent(in) :: expected

   !> A detailed message describing the error
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Another line of error message
   character(kind=tfc, len=*), intent(in), optional :: more

   if (expected /= actual) then
      if (present(message)) then
         call test_failed(error, message, more)
      else
         call test_failed(error, "Integer value missmatch", more)
      end if
   end if

end subroutine check_int_i8


subroutine check_bool(error, actual, expected, message, more)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Found boolean value
   logical, intent(in) :: actual

   !> Expected boolean value
   logical, intent(in) :: expected

   !> A detailed message describing the error
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Another line of error message
   character(kind=tfc, len=*), intent(in), optional :: more

   if (expected .neqv. actual) then
      if (present(message)) then
         call test_failed(error, message, more)
      else
         call test_failed(error, "Integer value missmatch", more)
      end if
   end if

end subroutine check_bool


end module tftest_testsuite
