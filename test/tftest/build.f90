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

!> Testsuite for the `tomlf_build` modules
module tftest_build
   use tftest_testsuite
   use tomlf_build
   use tomlf
   implicit none
   private

   public :: collect_build


contains


!> Collect all exported unit tests
subroutine collect_build(testsuite)

   !> Collection of tests
   type(toml_unittest), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("table-real-sp", table_real_sp), &
      & new_unittest("table-real-dp", table_real_dp), &
      & new_unittest("table-int-i1", table_int_i1), &
      & new_unittest("table-int-i2", table_int_i2), &
      & new_unittest("table-int-i4", table_int_i4), &
      & new_unittest("table-int-i8", table_int_i8), &
      & new_unittest("table-bool", table_bool)]

end subroutine collect_build


!> Check double precision floating point numbers (default)
subroutine table_real_dp(error)
   use tomlf_constants, only : tf_dp
   use tomlf_type, only : new_table, toml_keyval

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_table) :: table
   real(tf_dp), parameter :: in1 = 1.0_tf_dp, in2 = huge(in1), in3 = tiny(in1)
   real(tf_dp) :: val
   integer :: stat

   call new_table(table)
   call set_value(table, "real", in2, stat=stat)
   call get_value(table, "real", val, stat=stat)

   call check(error, val, in2, rel=.true.)
   if (allocated(error)) return

   call table%delete("real")
   call set_value(table, "real", in1, stat=stat)
   call get_value(table, "real", val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "real", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

end subroutine table_real_dp


!> Check single precision floating point numbers
!
!  Since TOML requires to store them as double precision numbers, we might find
!  larger deviations here than just epsilon.
subroutine table_real_sp(error)
   use tomlf_constants, only : tf_sp
   use tomlf_type, only : new_table, toml_keyval

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_table) :: table
   real(tf_sp), parameter :: in1 = 1.0_tf_sp, in2 = huge(in1), in3 = tiny(in1)
   real(tf_sp), parameter :: thr = 2*epsilon(0.0_tf_sp)
   real(tf_sp) :: val
   integer :: stat

   call new_table(table)
   call set_value(table, "real", in2, stat=stat)
   call get_value(table, "real", val, stat=stat)

   call check(error, val, in2, thr=thr, rel=.true.)
   if (allocated(error)) return

   call table%delete("real")
   call set_value(table, "real", in1, stat=stat)
   call get_value(table, "real", val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "real", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

end subroutine table_real_sp


!> Check double precision floating point numbers (default)
subroutine table_int_i1(error)
   use tomlf_constants, only : tf_i1
   use tomlf_type, only : new_table, toml_keyval

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_table) :: table
   integer(tf_i1), parameter :: in1 = 1_tf_i1, in2 = huge(in1), in3 = -huge(in1)
   integer(tf_i1) :: val
   integer :: stat

   call new_table(table)
   call set_value(table, "int", in2, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in2)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, "int", in1, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "int", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

end subroutine table_int_i1


!> Check double precision floating point numbers (default)
subroutine table_int_i2(error)
   use tomlf_constants, only : tf_i2
   use tomlf_type, only : new_table, toml_keyval

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_table) :: table
   integer(tf_i2), parameter :: in1 = 1_tf_i2, in2 = huge(in1), in3 = -huge(in1)
   integer(tf_i2) :: val
   integer :: stat

   call new_table(table)
   call set_value(table, "int", in2, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in2)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, "int", in1, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "int", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

end subroutine table_int_i2


!> Check double precision floating point numbers (default)
subroutine table_int_i4(error)
   use tomlf_constants, only : tf_i4
   use tomlf_type, only : new_table, toml_keyval

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_table) :: table
   integer(tf_i4), parameter :: in1 = 1_tf_i4, in2 = huge(in1), in3 = -huge(in1)
   integer(tf_i4) :: val
   integer :: stat

   call new_table(table)
   call set_value(table, "int", in2, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in2)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, "int", in1, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "int", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

end subroutine table_int_i4


!> Check double precision floating point numbers (default)
subroutine table_int_i8(error)
   use tomlf_constants, only : tf_i8
   use tomlf_type, only : new_table, toml_keyval

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_table) :: table
   integer(tf_i8), parameter :: in1 = 1_tf_i8, in2 = huge(in1), in3 = -huge(in1)
   integer(tf_i8) :: val
   integer :: stat

   call new_table(table)
   call set_value(table, "int", in2, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in2)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, "int", in1, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "int", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

end subroutine table_int_i8


!> Check double precision floating point numbers (default)
subroutine table_bool(error)
   use tomlf_type, only : new_table, toml_keyval

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_table) :: table
   logical, parameter :: true = .true., false = .false.
   logical :: val
   integer :: stat

   call new_table(table)
   call set_value(table, "logic", true, stat=stat)
   call get_value(table, "logic", val, stat=stat)

   call check(error, val, true)
   if (allocated(error)) return

   call table%delete("logic")
   call set_value(table, "logic", false, stat=stat)
   call get_value(table, "logic", val, stat=stat)

   call check(error, val, false)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "logic", val, true, stat=stat)

   call check(error, val, true)
   if (allocated(error)) return

end subroutine table_bool


end module tftest_build
