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

!> Testsuite for the `tomlf_build` modules
module tftest_build
   use testdrive
   use tomlf_build
   use tomlf
   implicit none
   private

   public :: collect_build


contains


!> Collect all exported unit tests
subroutine collect_build(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("array-real-sp", array_real_sp), &
      & new_unittest("array-real-dp", array_real_dp), &
      & new_unittest("array-int-i1", array_int_i1), &
      & new_unittest("array-int-i2", array_int_i2), &
      & new_unittest("array-int-i4", array_int_i4), &
      & new_unittest("array-int-i8", array_int_i8), &
      & new_unittest("array-bool", array_bool), &
      & new_unittest("array-datetime", array_datetime), &
      & new_unittest("array-string", array_string), &
      & new_unittest("array-merge", array_merge), &
      & new_unittest("table-array", table_array), &
      & new_unittest("table-real-sp", table_real_sp), &
      & new_unittest("table-real-dp", table_real_dp), &
      & new_unittest("table-int-i1", table_int_i1), &
      & new_unittest("table-int-i2", table_int_i2), &
      & new_unittest("table-int-i4", table_int_i4), &
      & new_unittest("table-int-i8", table_int_i8), &
      & new_unittest("table-bool", table_bool), &
      & new_unittest("table-dateime", table_datetime), &
      & new_unittest("table-string", table_string), &
      & new_unittest("table-merge-append", table_merge_append), &
      & new_unittest("table-merge-overwrite", table_merge_overwrite)]

end subroutine collect_build


!> Check double precision floating point numbers (default)
subroutine table_real_dp(error)
   use tomlf_constants, only : tf_dp, tfi
   use tomlf_type, only : new_table, toml_table, toml_key

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

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
   call set_value(table, toml_key("real"), in1, stat=stat)
   call get_value(table, toml_key("real"), val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%delete("real")
   call set_value(table, toml_path("real", "sub", "sub", "sub"), in1, stat=stat)
   call get_value(table, toml_path("real", "sub", "sub", "sub"), val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "real", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

   call set_value(table, "int", huge(1_tfi), stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, stat, toml_stat%conversion_error)
   if (allocated(error)) return

   call set_value(table, "int", 1_tfi, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, 1.0_tf_dp)
   if (allocated(error)) return

   call set_value(table, "str", "1", stat=stat)
   call get_value(table, "str", val, stat=stat)

   call check(error, stat, toml_stat%type_mismatch)
   if (allocated(error)) return

end subroutine table_real_dp


!> Check single precision floating point numbers
!>
!> Since TOML requires to store them as double precision numbers, we might find
!> larger deviations here than just epsilon.
subroutine table_real_sp(error)
   use tomlf_constants, only : tf_sp, tfi
   use tomlf_type, only : new_table, toml_table, toml_key

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   real(tf_sp), parameter :: in1 = 1.0_tf_sp, in2 = huge(in1), in3 = tiny(in1)
   real(tf_sp), parameter :: thr = 2*epsilon(0.0_tf_sp)
   real(tf_sp) :: val
   integer :: stat

   call new_table(table)
   call set_value(table, toml_key("real"), in2, stat=stat)
   call get_value(table, toml_key("real"), val, stat=stat)

   call check(error, val, in2, thr=thr, rel=.true.)
   if (allocated(error)) return

   call table%delete("real")
   call set_value(table, "real", in1, stat=stat)
   call get_value(table, "real", val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%delete("real")
   call set_value(table, toml_path("real", "sub"), in1, stat=stat)
   call get_value(table, toml_path("real", "sub"), val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "real", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

   call set_value(table, "int", huge(1_tfi), stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, stat, toml_stat%conversion_error)
   if (allocated(error)) return

   call set_value(table, "int", 1_tfi, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, 1.0_tf_sp)
   if (allocated(error)) return

   call set_value(table, "str", "1", stat=stat)
   call get_value(table, "str", val, stat=stat)

   call check(error, stat, toml_stat%type_mismatch)
   if (allocated(error)) return

end subroutine table_real_sp


!> Check smallest integers (char)
subroutine table_int_i1(error)
   use tomlf_constants, only : tf_i1, tfi
   use tomlf_type, only : new_table, toml_table, toml_key

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   integer(tf_i1), parameter :: in1 = 1_tf_i1, in2 = huge(in1), in3 = -huge(in1)
   integer(tf_i1) :: val
   integer :: stat

   ! table uninitialized
   call set_value(table, toml_key("int"), in2, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in2)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, "int", in1, stat=stat)
   call get_value(table, toml_key("int"), val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, toml_path("int", "sub"), in1, stat=stat)
   call get_value(table, toml_path("int", "sub"), val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "int", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

   call set_value(table, "huge", huge(1_tfi), stat=stat)
   call get_value(table, "huge", val, stat=stat)

   call check(error, stat, toml_stat%conversion_error)
   if (allocated(error)) return

   call set_value(table, "str", "1", stat=stat)
   call get_value(table, "str", val, stat=stat)

   call check(error, stat, toml_stat%type_mismatch)
   if (allocated(error)) return

   call get_value(table, "missing", val, stat=stat)
   call check(error, stat, toml_stat%missing_key)
   if (allocated(error)) return

end subroutine table_int_i1


!> Check short integers
subroutine table_int_i2(error)
   use tomlf_constants, only : tf_i2, tfi
   use tomlf_type, only : new_table, toml_table

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   integer(tf_i2), parameter :: in1 = 1_tf_i2, in2 = huge(in1), in3 = -huge(in1)
   integer(tf_i2) :: val
   integer :: stat

   call new_table(table)
   call set_value(table, toml_key("int"), in2, stat=stat)
   call get_value(table, toml_key("int"), val, stat=stat)

   call check(error, val, in2)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, "int", in1, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, toml_path("int", "sub"), in1, stat=stat)
   call get_value(table, toml_path("int", "sub"), val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "int", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

   call set_value(table, "huge", huge(1_tfi), stat=stat)
   call get_value(table, "huge", val, stat=stat)

   call check(error, stat, toml_stat%conversion_error)
   if (allocated(error)) return

   call set_value(table, "str", "1", stat=stat)
   call get_value(table, "str", val, stat=stat)

   call check(error, stat, toml_stat%type_mismatch)
   if (allocated(error)) return

   call get_value(table, "missing", val, stat=stat)
   call check(error, stat, toml_stat%missing_key)
   if (allocated(error)) return

end subroutine table_int_i2


!> Check default integers
subroutine table_int_i4(error)
   use tomlf_constants, only : tf_i4, tfi
   use tomlf_type, only : new_table, toml_table, toml_key

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

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
   call set_value(table, toml_key("int"), in1, stat=stat)
   call get_value(table, toml_key("int"), val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, toml_path("int", "sub", "sub"), in1, stat=stat)
   call get_value(table, toml_path("int", "sub", "sub"), val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "int", val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

   call set_value(table, "huge", huge(1_tfi), stat=stat)
   call get_value(table, "huge", val, stat=stat)

   call check(error, stat, toml_stat%conversion_error)
   if (allocated(error)) return

   call set_value(table, "str", "1", stat=stat)
   call get_value(table, "str", val, stat=stat)

   call check(error, stat, toml_stat%type_mismatch)
   if (allocated(error)) return

   call get_value(table, "missing", val, stat=stat)
   call check(error, stat, toml_stat%missing_key)
   if (allocated(error)) return

end subroutine table_int_i4


!> Check long integers (default)
subroutine table_int_i8(error)
   use tomlf_constants, only : tf_i8
   use tomlf_type, only : new_table, toml_table, toml_key
   use tomlf_utils, only : to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   integer :: ii
   integer(tf_i8), parameter :: in1 = 1_tf_i8, in2 = huge(in1), in3 = -huge(in1)
   integer(tf_i8) :: val
   integer :: stat

   call new_table(table)
   call set_value(table, "int", in2, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in2)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, toml_key("int"), in1, stat=stat)
   call get_value(table, "int", val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%delete("int")
   call set_value(table, toml_path([toml_key("int"), toml_key("sub")]), in1, stat=stat)
   call get_value(table, toml_path([toml_key("int"), toml_key("sub")]), val, stat=stat)

   call check(error, val, in1)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, toml_key("int"), val, in3, stat=stat)

   call check(error, val, in3)
   if (allocated(error)) return

   call set_value(table, "str", "1", stat=stat)
   call get_value(table, "str", val, stat=stat)

   call check(error, stat, toml_stat%type_mismatch)
   if (allocated(error)) return

   call table%destroy()
   call new_table(table)
   do ii = 1, 100
      call set_value(table, to_string(ii), int(ii, tf_i8), stat=stat)
   end do
   call get_value(table, to_string(100), val, stat=stat)
   call check(error, val, 100_tf_i8)
   if (allocated(error)) return

   call get_value(table, "missing", val, stat=stat)
   call check(error, stat, toml_stat%missing_key)
   if (allocated(error)) return

end subroutine table_int_i8


!> Check logicals
subroutine table_bool(error)
   use tomlf_type, only : toml_table, toml_key

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   logical, parameter :: true = .true., false = .false.
   logical :: val
   integer :: stat

   table = toml_table()
   call set_value(table, toml_key("logic"), true, stat=stat)
   call get_value(table, toml_key("logic"), val, stat=stat)

   call check(error, val, true)
   if (allocated(error)) return

   call table%delete("logic")
   call set_value(table, "logic", false, stat=stat)
   call get_value(table, "logic", val, stat=stat)

   call check(error, val, false)
   if (allocated(error)) return

   call table%delete("logic")
   call set_value(table, toml_path("logic", "sub"), false, stat=stat)
   call get_value(table, toml_path("logic", "sub"), val, stat=stat)

   call check(error, val, false)
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "logic", val, true, stat=stat)

   call check(error, val, true)
   if (allocated(error)) return

   call set_value(table, "str", "1", stat=stat)
   call get_value(table, "str", val, stat=stat)

   call check(error, stat, toml_stat%type_mismatch)
   if (allocated(error)) return

   call get_value(table, "missing", val, stat=stat)
   call check(error, stat, toml_stat%missing_key)
   if (allocated(error)) return

end subroutine table_bool


!> Check datetime
subroutine table_datetime(error)
   use tomlf_type, only : toml_table, toml_key
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time, operator(==), to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   type(toml_datetime) :: val, ts1, ts2
   integer :: stat

   ts1 = toml_datetime(toml_date(2022,  7, 31), toml_time(13, 51, 42))
   ts2 = toml_datetime(toml_date(2019, 12, 17), toml_time(18, 26, 59))

   table = toml_table()
   call set_value(table, toml_key("datetime"), ts1, stat=stat)
   call get_value(table, toml_key("datetime"), val, stat=stat)

   call check(error, val == ts1, &
      & "Expected '"//to_string(ts1)//"' but got '"//to_string(val)//"'")
   if (allocated(error)) return

   call table%delete("datetime")
   call set_value(table, "datetime", ts2, stat=stat)
   call get_value(table, "datetime", val, stat=stat)

   call check(error, val == ts2, &
      & "Expected '"//to_string(ts2)//"' but got '"//to_string(val)//"'")
   if (allocated(error)) return

   call table%delete("datetime")
   call set_value(table, toml_path("datetime", "sub", "sub"), ts2, stat=stat)
   call get_value(table, toml_path("datetime", "sub", "sub"), val, stat=stat)

   call check(error, val == ts2, &
      & "Expected '"//to_string(ts2)//"' but got '"//to_string(val)//"'")
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, "datetime", val, ts1, stat=stat)

   call check(error, val == ts1, &
      & "Expected '"//to_string(ts1)//"' but got '"//to_string(val)//"'")
   if (allocated(error)) return

   call set_value(table, "str", "1", stat=stat)
   call get_value(table, "str", val, stat=stat)

   call check(error, stat, toml_stat%type_mismatch)
   if (allocated(error)) return

   call get_value(table, "missing", val, stat=stat)
   call check(error, stat, toml_stat%missing_key)
   if (allocated(error)) return

end subroutine table_datetime


!> Check strings
subroutine table_string(error)
   use tomlf_type, only : toml_table, toml_key

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   character(len=:), allocatable :: val
   integer :: stat

   table = toml_table()
   call set_value(table, toml_key("string"), "value", stat=stat)
   call get_value(table, "string", val, stat=stat)

   call check(error, val, "value")
   if (allocated(error)) return

   call table%delete("string")
   call set_value(table, "string", """value""", stat=stat)
   call get_value(table, "string", val, stat=stat)

   call check(error, val, """value""")
   if (allocated(error)) return

   call table%delete("string")
   call set_value(table, toml_path("string", "sub"), "value", stat=stat)
   call get_value(table, toml_path("string", "sub"), val, stat=stat)

   call check(error, val, "value")
   if (allocated(error)) return

   call table%destroy
   call new_table(table)
   call get_value(table, toml_key("string"), val, "'value'", stat=stat)

   call check(error, val, "'value'")
   if (allocated(error)) return

   call get_value(table, "missing", val, stat=stat)
   call check(error, stat, toml_stat%missing_key)
   if (allocated(error)) return

end subroutine table_string

subroutine table_array(error)
   use tomlf_type, only : toml_value, new_table, toml_table, add_table, new_array, &
      & toml_array, toml_key, add_array, len

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   type(toml_table), pointer :: child
   type(toml_array), pointer :: children
   class(toml_value), allocatable :: val
   integer :: i, stat

   call new_table(table)
   call get_value(table, toml_key("array-of-tables"), children, &
      & requested=.false., stat=stat)
   call check(error, stat, "Not finding a not required value shouldn't be an error")
   if (allocated(error)) return

   call add_array(table, "array-of-tables", children, stat)

   do i = 1, 4
      call add_table(children, child, stat)
      call set_value(child, "val", i*i)
   end do

   call check(error, len(children), 4)
   if (allocated(error)) return

   call children%pop(val)

   call check(error, len(children), 3)
   if (allocated(error)) return

   select type(val)
   class default
      call test_failed(error, "Popped value is not of table type")
   class is (toml_table)
      call get_value(val, "val", i)
      call check(error, i, 16)
   end select
   if (allocated(error)) return

   call children%shift(val)

   call check(error, len(children), 2)
   if (allocated(error)) return

   select type(val)
   class default
      call test_failed(error, "Shifted value is not of table type")
   class is (toml_table)
      call get_value(val, "val", i)
      call check(error, i, 1)
   end select
   if (allocated(error)) return

   call get_value(table, toml_path("tables", "with", "array"), children, &
      & requested=.false., stat=stat)
   call check(error, .not.associated(children), "Should not create array")
   if (allocated(error)) return
   call check(error, stat, "Not finding a not required value shouldn't be an error")
   if (allocated(error)) return

   call get_value(table, toml_path("tables", "with", "array"), children, &
      & requested=.true.)
   call check(error, associated(children), "Array was not created")
   if (allocated(error)) return
end subroutine table_array


!> Check single precision floating point numbers
subroutine array_real_sp(error)
   use tomlf_constants, only : tf_sp
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_array) :: array
   real(tf_sp) :: val
   real(tf_sp), allocatable :: vals(:)
   integer :: ii
   integer :: stat

   ! array not initialized
   do ii = 1, 10
      call set_value(array, ii, sqrt(real(ii, tf_sp)), stat=stat)
   end do
   call check(error, len(array), 10)
   if (allocated(error)) return

   ii = 7
   call get_value(array, ii, val, stat=stat)
   call check(error, val, sqrt(7.0_tf_sp), rel=.true.)
   if (allocated(error)) return

   call get_value(array, vals, stat=stat)
   call check(error, allocated(vals))
   call check(error, vals(7), val, rel=.true.)
   if (allocated(error)) return

   vals = 2*vals(:9)
   call set_value(array, vals, stat=stat)
   call check(error, len(array), 9)
   if (allocated(error)) return

   call get_value(array, ii, val, stat=stat)
   call check(error, val, 2*sqrt(7.0_tf_sp), rel=.true.)
   if (allocated(error)) return

end subroutine array_real_sp


!> Check double precision floating point numbers (default)
subroutine array_real_dp(error)
   use tomlf_constants, only : tf_dp
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_array) :: array
   real(tf_dp) :: val
   real(tf_dp), allocatable :: vals(:)
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

   call get_value(array, vals, stat=stat)
   call check(error, allocated(vals))
   call check(error, vals(7), val, rel=.true.)
   if (allocated(error)) return

   vals = 2*vals(:9)
   call set_value(array, vals, stat=stat)
   call check(error, len(array), 9)
   if (allocated(error)) return

   call get_value(array, ii, val, stat=stat)
   call check(error, val, 2*sqrt(7.0_tf_dp), rel=.true.)
   if (allocated(error)) return

end subroutine array_real_dp


!> Check smallest integers (char)
subroutine array_int_i1(error)
   use tomlf_constants, only : tf_i1
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_array) :: array
   integer(tf_i1) :: val
   integer(tf_i1), allocatable :: vals(:)
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

   call get_value(array, vals, stat=stat)
   call check(error, allocated(vals))
   call check(error, vals(ii), val)
   if (allocated(error)) return

   vals = -vals(:ii)
   call set_value(array, vals, stat=stat)
   call check(error, len(array), ii)
   if (allocated(error)) return

   call get_value(array, ii, val, stat=stat)
   call check(error, val, -ref(ii))
   if (allocated(error)) return

end subroutine array_int_i1


!> Check short integers
subroutine array_int_i2(error)
   use tomlf_constants, only : tf_i2
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_array) :: array
   integer(tf_i2) :: val
   integer(tf_i2), allocatable :: vals(:)
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

   call get_value(array, vals, stat=stat)
   call check(error, allocated(vals))
   call check(error, vals(ii), val)
   if (allocated(error)) return

   vals = -vals(:ii)
   call set_value(array, vals, stat=stat)
   call check(error, len(array), ii)
   if (allocated(error)) return

   call get_value(array, ii, val, stat=stat)
   call check(error, val, -ref(ii))
   if (allocated(error)) return

end subroutine array_int_i2


!> Check default integers
subroutine array_int_i4(error)
   use tomlf_constants, only : tf_i4
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_array) :: array
   integer(tf_i4) :: val
   integer(tf_i4), allocatable :: vals(:)
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

   call get_value(array, vals, stat=stat)
   call check(error, allocated(vals))
   call check(error, vals(ii), val)
   if (allocated(error)) return

   vals = -vals(:ii)
   call set_value(array, vals, stat=stat)
   call check(error, len(array), ii)
   if (allocated(error)) return

   call get_value(array, ii, val, stat=stat)
   call check(error, val, -ref(ii))
   if (allocated(error)) return

end subroutine array_int_i4


!> Check long integers (default)
subroutine array_int_i8(error)
   use tomlf_constants, only : tf_i8
   use tomlf_type, only : new_array, toml_array, len

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_array) :: array
   integer(tf_i8) :: val
   integer(tf_i8), allocatable :: vals(:)
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

   call get_value(array, vals, stat=stat)
   call check(error, allocated(vals))
   call check(error, vals(ii), val)
   if (allocated(error)) return

   vals = -vals(:ii)
   call set_value(array, vals, stat=stat)
   call check(error, len(array), ii)
   if (allocated(error)) return

   call get_value(array, ii, val, stat=stat)
   call check(error, val, -ref(ii))
   if (allocated(error)) return

   call array%destroy()
   call new_array(array)
   do ii = 1, 100
      call set_value(array, ii, int(ii, tf_i8), stat=stat)
   end do
   call check(error, len(array), 100)
   if (allocated(error)) return

end subroutine array_int_i8


!> Check logicals
subroutine array_bool(error)
   use tomlf_type, only : toml_array, len

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_array) :: array
   logical :: val
   logical, allocatable :: vals(:)
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

   call get_value(array, vals, stat=stat)
   call check(error, allocated(vals))
   call check(error, vals(7), val)
   if (allocated(error)) return

   vals = .not.vals(:9)
   call set_value(array, vals, stat=stat)
   call check(error, len(array), 9)
   if (allocated(error)) return

   call get_value(array, ii, val, stat=stat)
   call check(error, val, mod(ii, 2) /= 1)
   if (allocated(error)) return

end subroutine array_bool


!> Check datetime
subroutine array_datetime(error)
   use tomlf_type, only : toml_array, len
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time, operator(==), to_string

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_array) :: array
   type(toml_datetime) :: val, ts
   type(toml_datetime), allocatable :: vals(:)
   integer :: ii
   integer :: stat

   array = toml_array()
   do ii = 1, 10
      call set_value(array, ii, toml_datetime(toml_date(2022, ii, 7), toml_time()), stat=stat)
   end do
   call check(error, len(array), 10)
   if (allocated(error)) return

   ii = 3
   call get_value(array, ii, val, stat=stat)
   ts = toml_datetime(toml_date(2022, ii, 7), toml_time())
   call check(error, val == ts, &
      & "Expected '"//to_string(ts)//"' but got '"//to_string(val)//"'")
   if (allocated(error)) return

   call get_value(array, vals, stat=stat)
   call check(error, vals(ii) == val, &
      & "Expected '"//to_string(val)//"' but got '"//to_string(vals(ii))//"'")
   if (allocated(error)) return

   vals = [(toml_datetime(toml_date(2022, ii, 8), toml_time()), ii = 1, 9)]
   call set_value(array, vals, stat=stat)
   call check(error, len(array), 9)
   if (allocated(error)) return

   call get_value(array, ii, val, stat=stat)
   call check(error, val == vals(ii), &
      & "Expected '"//to_string(vals(ii))//"' but got '"//to_string(val)//"'")
   if (allocated(error)) return

end subroutine array_datetime


!> Check datetime
subroutine array_string(error)
   use tomlf_type, only : toml_array, len

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_array) :: array
   character(:), allocatable :: val
   integer :: ii
   integer :: stat

   array = toml_array()
   do ii = 1, 10
      call set_value(array, ii, repeat("a", ii), stat=stat)
   end do
   call check(error, len(array), 10)
   if (allocated(error)) return

   ii = 3
   call get_value(array, ii, val, stat=stat)
   call check(error, val, repeat("a", ii))
   if (allocated(error)) return

end subroutine array_string


!> Merge two arrays
subroutine array_merge(error)
   use tomlf_type, only : toml_array, new_array, len

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_array) :: array1, array2
   integer :: val1, val2
   integer :: ii
   integer :: stat

   array1 = toml_array()

   do ii = 1, 4
      call set_value(array1, ii, ii*ii, stat=stat)
   end do

   call new_array(array2)

   do ii = 1, 6
      call set_value(array2, ii, ii*ii, stat=stat)
   end do

   call merge_array(array1, array2)
   call array2%destroy

   call check(error, len(array1), 10)
   if (allocated(error)) return

   call get_value(array1, 2, val1, stat)
   call get_value(array1, 6, val2, stat)

   call check(error, val1, val2)
   if (allocated(error)) return

end subroutine array_merge


!> Merge two tables
subroutine table_merge_append(error)
   use tomlf_type, only : toml_table, add_table, new_table, toml_key

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table1, table2
   type(toml_table), pointer :: child
   type(toml_key), allocatable :: list(:)
   real :: val
   integer :: stat

   table1 = toml_table()

   call set_value(table1, "first", 10, stat)
   call set_value(table1, "second", "string", stat)
   call set_value(table1, "third", 1.0e-7, stat)
   call add_table(table1, "fourth", child, stat)
   call set_value(child, "child", .true., stat)

   call new_table(table2)

   call set_value(table2, "val", 12, stat)
   call set_value(table2, "key", "content", stat)
   call set_value(table2, "entry", -1.0e-7, stat)
   call set_value(table2, "third", .false., stat)
   call add_table(table2, "section", child, stat)
   call set_value(child, "sub", .false., stat)
   call add_table(table2, "fourth", child, stat)

   call merge_table(table1, table2)
   call table2%destroy

   call table1%get_keys(list)

   call check(error, size(list), 8)
   if (allocated(error)) return

   call get_value(table1, "third", val, stat=stat)
   call check(error, stat)
   if (allocated(error)) return

end subroutine table_merge_append


!> Merge two tables
subroutine table_merge_overwrite(error)
   use tomlf_type, only : toml_table, add_table, new_table, toml_key

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table1, table2
   type(toml_table), pointer :: child
   type(toml_key), allocatable :: list(:)
   logical :: val
   integer :: stat

   table1 = toml_table()

   call set_value(table1, "first", 10, stat)
   call set_value(table1, "second", "string", stat)
   call set_value(table1, "third", 1.0e-7, stat)
   call add_table(table1, "fourth", child, stat)
   call set_value(child, "child", .true., stat)

   call new_table(table2)

   call set_value(table2, "val", 12, stat)
   call set_value(table2, "key", "content", stat)
   call set_value(table2, "entry", -1.0e-7, stat)
   call set_value(table2, "third", .false., stat)
   call add_table(table2, "section", child, stat)
   call set_value(child, "sub", .false., stat)
   call add_table(table2, "fourth", child, stat)

   call merge_table(table1, table2, toml_merge_config(keyval="overwrite"))
   call table2%destroy

   call table1%get_keys(list)

   call check(error, size(list), 8)
   if (allocated(error)) return

   call get_value(table1, "third", val, stat=stat)
   call check(error, stat)
   if (allocated(error)) return

end subroutine table_merge_overwrite


end module tftest_build
