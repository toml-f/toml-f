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

!> Wrapper for the testsuites
program tftester
   use, intrinsic :: iso_fortran_env, only : error_unit
   use testdrive, only : run_testsuite
   use tftest_build, only : collect_build
   use tftest_sort, only : collect_sort
   implicit none
   integer :: stat
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   write(error_unit, fmt) repeat('-', 72)
   write(error_unit, fmt) "Testing:", "build"
   call run_testsuite(collect_build, error_unit, stat)

   if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "tests failed!"
      error stop 1
   end if

   write(error_unit, fmt) repeat('-', 72)
   write(error_unit, fmt) "Testing:", "sort"
   call run_testsuite(collect_sort, error_unit, stat)

   if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "tests failed!"
      error stop 1
   end if

end program tftester
