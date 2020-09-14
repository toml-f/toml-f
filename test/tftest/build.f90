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
      & new_unittest("array-real-sp", array_real_sp), &
      & new_unittest("array-real-dp", array_real_dp), &
      & new_unittest("array-int-i1", array_int_i1), &
      & new_unittest("array-int-i2", array_int_i2), &
      & new_unittest("array-int-i4", array_int_i4), &
      & new_unittest("array-int-i8", array_int_i8), &
      & new_unittest("array-bool", array_bool), &
      & new_unittest("table-real-sp", table_real_sp), &
      & new_unittest("table-real-dp", table_real_dp), &
      & new_unittest("table-int-i1", table_int_i1), &
      & new_unittest("table-int-i2", table_int_i2), &
      & new_unittest("table-int-i4", table_int_i4), &
      & new_unittest("table-int-i8", table_int_i8), &
      & new_unittest("table-bool", table_bool), &
      & new_unittest("table-string", table_string)]

end subroutine collect_build


!> Check double precision floating point numbers (default)
subroutine table_real_dp(error)
   use tomlf_constants, only : tf_dp
   use tomlf_type, only : new_table, toml_table

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
   use tomlf_type, only : new_table, toml_table

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
   use tomlf_type, only : new_table, toml_table

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
   use tomlf_type, only : new_table, toml_table

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
   use tomlf_type, only : new_table, toml_table

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
   use tomlf_type, only : new_table, toml_table

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
   use tomlf_type, only : toml_table

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_table) :: table
   logical, parameter :: true = .true., false = .false.
   logical :: val
   integer :: stat

   table = toml_table()
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


!> Check double precision floating point numbers (default)
subroutine table_string(error)
   use tomlf_type, only : toml_table

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_table) :: table
   character(len=:), allocatable :: val
   integer :: stat
   type(toml_serializer) :: ser

   table = toml_table()
   call set_value(table, "string", "value", stat=stat)
   call get_value(table, "string", val, stat=stat)

   call check(error, val, "value")
   if (allocated(error)) return

   call table%delete("string")
   call set_value(table, "string", """value""", stat=stat)
   call get_value(table, "string", val, stat=stat)

   call table%accept(ser)
   call check(error, val, "value")
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "string", val, "'value'", stat=stat)

   call check(error, val, "value")
   if (allocated(error)) return

end subroutine table_string


!> Check double precision floating point numbers (default)
subroutine array_real_sp(error)
   use tomlf_constants, only : tf_sp
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_array) :: array
   real(tf_sp) :: val
   integer :: ii
   integer :: stat

   call new_array(array)
   do ii = 1, 10
      call set_value(array, ii, sqrt(real(ii, tf_sp)), stat=stat)
   end do
   call check(error, len(array), 10)
   if (allocated(error)) return

   ii = 7
   call get_value(array, ii, val, stat=stat)
   call check(error, val, sqrt(7.0_tf_sp), rel=.true.)
   if (allocated(error)) return

end subroutine array_real_sp


!> Check double precision floating point numbers (default)
subroutine array_real_dp(error)
   use tomlf_constants, only : tf_dp
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_array) :: array
   real(tf_dp) :: val
   integer :: ii
   integer :: stat

   call new_array(array)
   do ii = 1, 10
      call set_value(array, ii, sqrt(real(ii, tf_dp)), stat=stat)
   end do
   call check(error, len(array), 10)
   if (allocated(error)) return

   ii = 7
   call get_value(array, ii, val, stat=stat)
   call check(error, val, sqrt(7.0_tf_dp), rel=.true.)
   if (allocated(error)) return

end subroutine array_real_dp


!> Check double precision floating point numbers (default)
subroutine array_int_i1(error)
   use tomlf_constants, only : tf_i1
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_array) :: array
   integer(tf_i1) :: val
   integer(tf_i1), parameter :: ref(4) = [integer(tf_i1) :: 1, 3, 5, 7]
   integer :: ii
   integer :: stat

   call new_array(array)
   do ii = 1, size(ref)
      call set_value(array, ii, ref(ii), stat=stat)
   end do
   call check(error, len(array), size(ref))
   if (allocated(error)) return

   ii = 3
   call get_value(array, ii, val, stat=stat)
   call check(error, val, ref(ii))
   if (allocated(error)) return

end subroutine array_int_i1


!> Check double precision floating point numbers (default)
subroutine array_int_i2(error)
   use tomlf_constants, only : tf_i2
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_array) :: array
   integer(tf_i2) :: val
   integer(tf_i2), parameter :: ref(4) = [integer(tf_i2) :: 1, 3, 5, 7]
   integer :: ii
   integer :: stat

   call new_array(array)
   do ii = 1, size(ref)
      call set_value(array, ii, ref(ii), stat=stat)
   end do
   call check(error, len(array), size(ref))
   if (allocated(error)) return

   ii = 3
   call get_value(array, ii, val, stat=stat)
   call check(error, val, ref(ii))
   if (allocated(error)) return

end subroutine array_int_i2


!> Check double precision floating point numbers (default)
subroutine array_int_i4(error)
   use tomlf_constants, only : tf_i4
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_array) :: array
   integer(tf_i4) :: val
   integer(tf_i4), parameter :: ref(4) = [integer(tf_i4) :: 1, 3, 5, 7]
   integer :: ii
   integer :: stat

   call new_array(array)
   do ii = 1, size(ref)
      call set_value(array, ii, ref(ii), stat=stat)
   end do
   call check(error, len(array), size(ref))
   if (allocated(error)) return

   ii = 3
   call get_value(array, ii, val, stat=stat)
   call check(error, val, ref(ii))
   if (allocated(error)) return

end subroutine array_int_i4


!> Check double precision floating point numbers (default)
subroutine array_int_i8(error)
   use tomlf_constants, only : tf_i8
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_array) :: array
   integer(tf_i8) :: val
   integer(tf_i8), parameter :: ref(4) = [integer(tf_i8) :: 1, 3, 5, 7]
   integer :: ii
   integer :: stat

   call new_array(array)
   do ii = 1, size(ref)
      call set_value(array, ii, ref(ii), stat=stat)
   end do
   call check(error, len(array), size(ref))
   if (allocated(error)) return

   ii = 3
   call get_value(array, ii, val, stat=stat)
   call check(error, val, ref(ii))
   if (allocated(error)) return

end subroutine array_int_i8


!> Check double precision floating point numbers (default)
subroutine array_bool(error)
   use tomlf_type, only : toml_array, len

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   type(toml_array) :: array
   logical :: val
   integer :: ii
   integer :: stat

   array = toml_array()
   do ii = 1, 10
      call set_value(array, ii, mod(ii, 2) == 1, stat=stat)
   end do
   call check(error, len(array), 10)
   if (allocated(error)) return

   ii = 3
   call get_value(array, ii, val, stat=stat)
   call check(error, val, mod(ii, 2) == 1)
   if (allocated(error)) return

end subroutine array_bool


end module tftest_build
