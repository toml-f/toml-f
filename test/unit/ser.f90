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

module tftest_ser
   use testdrive
   use tomlf_constants, only : tfi, tfr
   use tomlf_de, only : toml_parse, toml_loads
   use tomlf_ser, only : toml_dumps
   use tomlf_type, only : toml_table, toml_array, new_table, add_table, add_array
   use tomlf_build, only : set_value
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time
   use tomlf_error, only : toml_error
   implicit none
   private

   public :: collect_ser


contains


!> Collect all exported unit tests
subroutine collect_ser(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("ser-simple-string", ser_simple_string), &
      & new_unittest("ser-integer", ser_integer), &
      & new_unittest("ser-float", ser_float), &
      & new_unittest("ser-boolean", ser_boolean), &
      & new_unittest("ser-array", ser_array), &
      & new_unittest("ser-table", ser_table), &
      & new_unittest("ser-roundtrip-simple", ser_roundtrip_simple)]

end subroutine collect_ser


!> Test serialization of simple string key-value pair
subroutine ser_simple_string(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call set_value(table, "key", "value")

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, len_trim(output) > 0, "Output should not be empty")
   if (allocated(error)) return

   call check(error, index(output, "key") > 0, "Output should contain 'key'")
end subroutine ser_simple_string


!> Test serialization of integer values
subroutine ser_integer(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call set_value(table, "int1", 42_tfi)
   call set_value(table, "int2", -123_tfi)
   call set_value(table, "int3", 0_tfi)

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, index(output, "42") > 0, "Output should contain '42'")
   if (allocated(error)) return

   call check(error, index(output, "-123") > 0, "Output should contain '-123'")
end subroutine ser_integer


!> Test serialization of float values
subroutine ser_float(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call set_value(table, "pi", 3.14159_tfr)
   call set_value(table, "negative", -2.5_tfr)
   call set_value(table, "zero", 0.0_tfr)

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, index(output, "3.14") > 0, "Output should contain float value")
end subroutine ser_float


!> Test serialization of boolean values
subroutine ser_boolean(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call set_value(table, "bool_true", .true.)
   call set_value(table, "bool_false", .false.)

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, index(output, "true") > 0, "Output should contain 'true'")
   if (allocated(error)) return

   call check(error, index(output, "false") > 0, "Output should contain 'false'")
end subroutine ser_boolean


!> Test serialization of simple array
subroutine ser_array(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   type(toml_array), pointer :: array
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call add_array(table, "numbers", array)
   call set_value(array, 1, 1_tfi)
   call set_value(array, 2, 2_tfi)
   call set_value(array, 3, 3_tfi)

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, index(output, "[") > 0, "Output should contain array bracket")
end subroutine ser_array


!> Test serialization of simple table
subroutine ser_table(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table) :: table
   type(toml_table), pointer :: subtable
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err

   call new_table(table)
   call add_table(table, "section", subtable)
   call set_value(subtable, "key", "value")

   call toml_dumps(table, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, toml_err%message)
      return
   end if

   call check(error, index(output, "[section]") > 0, "Output should contain table header")
end subroutine ser_table


!> Test round-trip serialization (parse -> serialize -> parse)
subroutine ser_roundtrip_simple(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_table), allocatable :: table1, table2
   character(:), allocatable :: output
   type(toml_error), allocatable :: toml_err
   character(*), parameter :: input = 'key = "value"'//new_line('a')//'number = 42'

   ! Parse input
   call toml_loads(table1, input, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Parse failed: "//toml_err%message)
      return
   end if

   ! Serialize
   call toml_dumps(table1, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Serialize failed: "//toml_err%message)
      return
   end if

   ! Parse again
   call toml_loads(table2, output, error=toml_err)
   if (allocated(toml_err)) then
      call test_failed(error, "Re-parse failed: "//toml_err%message)
      return
   end if

   call check(error, allocated(table2), "Round-trip should produce valid table")
end subroutine ser_roundtrip_simple


end module tftest_ser
