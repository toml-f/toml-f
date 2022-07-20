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

module tftest_utils
   use testdrive
   implicit none
   private

   public :: collect_utils


contains


!> Collect all exported unit tests
subroutine collect_utils(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("string-i1", string_i1), &
      & new_unittest("string-i2", string_i2), &
      & new_unittest("string-i4", string_i4), &
      & new_unittest("string-i8", string_i8)]

end subroutine collect_utils


subroutine string_i1(error)
   use tomlf_constants, only : tf_i1
   use tomlf_utils, only : to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, to_string(-huge(1_tf_i1) - 1_tf_i1), "-128")
end subroutine string_i1


subroutine string_i2(error)
   use tomlf_constants, only : tf_i2
   use tomlf_utils, only : to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, to_string(-huge(1_tf_i2) - 1_tf_i2), "-32768")
end subroutine string_i2


subroutine string_i4(error)
   use tomlf_constants, only : tf_i4
   use tomlf_utils, only : to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, to_string(-huge(1_tf_i4) - 1_tf_i4), "-2147483648")
end subroutine string_i4


subroutine string_i8(error)
   use tomlf_constants, only : tfi
   use tomlf_utils, only : to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, to_string(-huge(1_tfi) - 1_tfi), "-9223372036854775808")
end subroutine string_i8


end module tftest_utils
