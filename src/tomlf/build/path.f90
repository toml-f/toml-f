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

!> Support for retrieving and setting values using a key path.
module tomlf_build_path
   use tomlf_build_table, only : get_value, set_value
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp
   use tomlf_datetime, only : toml_datetime
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_table, toml_array, toml_keyval, toml_key
   implicit none
   private

   public :: toml_path, get_value, set_value


   !> Setter functions to manipulate TOML tables
   interface set_value
      module procedure :: set_path_value_float_sp
      module procedure :: set_path_value_float_dp
      module procedure :: set_path_value_integer_i1
      module procedure :: set_path_value_integer_i2
      module procedure :: set_path_value_integer_i4
      module procedure :: set_path_value_integer_i8
      module procedure :: set_path_value_bool
      module procedure :: set_path_value_datetime
      module procedure :: set_path_value_string
   end interface set_value


   !> Getter functions to manipulate TOML tables
   interface get_value
      module procedure :: get_path_table
      module procedure :: get_path_array
      module procedure :: get_path_keyval
      module procedure :: get_path_value_float_sp
      module procedure :: get_path_value_float_dp
      module procedure :: get_path_value_integer_i1
      module procedure :: get_path_value_integer_i2
      module procedure :: get_path_value_integer_i4
      module procedure :: get_path_value_integer_i8
      module procedure :: get_path_value_bool
      module procedure :: get_path_value_datetime
      module procedure :: get_path_value_string
   end interface get_value


   !> Wrapper for storing key paths
   type :: toml_path
      !> Path components
      type(toml_key), allocatable :: path(:)
   end type toml_path


   !> Convenience constructors for building key paths from strings instead of keys
   interface toml_path
      module procedure :: new_path2
      module procedure :: new_path3
      module procedure :: new_path4
   end interface toml_path


contains


!> Create a new path with two components
pure function new_path2(key1, key2) result(path)

   !> First key to retrieve
   character(*), intent(in) :: key1

   !> Second key to retrieve
   character(*), intent(in) :: key2

   !> New path
   type(toml_path) :: path

   allocate(path%path(2))
   path%path(:) = [toml_key(key1), toml_key(key2)]
end function new_path2


!> Create a new path with three components
pure function new_path3(key1, key2, key3) result(path)

   !> First key to retrieve
   character(*, tfc), intent(in) :: key1

   !> Second key to retrieve
   character(*, tfc), intent(in) :: key2

   !> Third key to retrieve
   character(*, tfc), intent(in) :: key3

   !> New path
   type(toml_path) :: path

   allocate(path%path(3))
   path%path(:) = [toml_key(key1), toml_key(key2), toml_key(key3)]
end function new_path3


!> Create a new path with three components
pure function new_path4(key1, key2, key3, key4) result(path)

   !> First key to retrieve
   character(*, tfc), intent(in) :: key1

   !> Second key to retrieve
   character(*, tfc), intent(in) :: key2

   !> Third key to retrieve
   character(*, tfc), intent(in) :: key3

   !> Forth key to retrieve
   character(*, tfc), intent(in) :: key4

   !> New path
   type(toml_path) :: path

   allocate(path%path(4))
   path%path(:) = [toml_key(key1), toml_key(key2), toml_key(key3), toml_key(key4)]
end function new_path4


subroutine get_path_table(table, path, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout), target :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child
   logical :: is_requested

   is_requested = .true.
   if (present(requested)) is_requested = requested

   nullify(ptr)
   call walk_path(table, path, child, is_requested, stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), ptr, is_requested, stat, origin)
   else
      if (.not.is_requested .and. present(stat)) stat = toml_stat%success
   end if
end subroutine get_path_table


subroutine get_path_array(table, path, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child
   logical :: is_requested

   is_requested = .true.
   if (present(requested)) is_requested = requested

   nullify(ptr)
   call walk_path(table, path, child, is_requested, stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), ptr, is_requested, stat, origin)
   else
      if (.not.is_requested .and. present(stat)) stat = toml_stat%success
   end if
end subroutine get_path_array


subroutine get_path_keyval(table, path, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child
   logical :: is_requested

   is_requested = .true.
   if (present(requested)) is_requested = requested

   nullify(ptr)
   call walk_path(table, path, child, is_requested, stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), ptr, is_requested, stat, origin)
   else
      if (.not.is_requested .and. present(stat)) stat = toml_stat%success
   end if
end subroutine get_path_keyval


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_path_value_float_sp(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Default real value
   real(tf_sp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_path_value_float_dp(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Default real value
   real(tf_dp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_path_value_integer_i1(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Default integer value
   integer(tf_i1), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_path_value_integer_i2(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Default integer value
   integer(tf_i2), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_path_value_integer_i4(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Default integer value
   integer(tf_i4), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_path_value_integer_i8(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Default integer value
   integer(tf_i8), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_path_value_bool(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Boolean value
   logical, intent(out) :: val

   !> Default boolean value
   logical, intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_bool


!> Retrieve TOML value as datetime
subroutine get_path_value_datetime(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Datetime value
   type(toml_datetime), intent(out) :: val

   !> Default datetime value
   type(toml_datetime), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_datetime


!> Retrieve TOML value as deferred-length character
subroutine get_path_value_string(table, path, val, default, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Default string value
   character(kind=tfc, len=*), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, present(default), stat, origin)
   if (associated(child)) then
      call get_value(child, path%path(size(path%path)), val, default, stat, origin)
   end if
end subroutine get_path_value_string


!> Set TOML value to single precision float
subroutine set_path_value_float_sp(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_float_sp


!> Set TOML value to double precision float
subroutine set_path_value_float_dp(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_float_dp


!> Set TOML value to one byte integer
subroutine set_path_value_integer_i1(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_path_value_integer_i2(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_path_value_integer_i4(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_path_value_integer_i8(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_integer_i8


!> Set TOML value to logical
subroutine set_path_value_bool(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_bool


!> Set TOML value to datetime
subroutine set_path_value_datetime(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Datetime value
   type(toml_datetime), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_datetime


!> Set TOML value to deferred-length character
subroutine set_path_value_string(table, path, val, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   type(toml_table), pointer :: child

   call walk_path(table, path, child, .true., stat, origin)
   if (associated(child)) then
      call set_value(child, path%path(size(path%path)), val, stat, origin)
   end if
end subroutine set_path_value_string


subroutine walk_path(table, path, ptr, requested, stat, origin)

   !> Instance of the TOML table
   class(toml_table), intent(inout), target :: table

   !> Path in this TOML table
   type(toml_path), intent(in) :: path

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   !> Origin in the data structure
   integer, intent(out), optional :: origin

   integer :: it
   type(toml_table), pointer :: current, next

   nullify(ptr)
   if (.not.allocated(path%path)) then
      if (present(stat)) stat = toml_stat%fatal
      if (present(origin)) origin = table%origin
      return
   end if

   current => table
   do it = 1, size(path%path) - 1
      call get_value(current, path%path(it)%key, next, requested, stat, origin)
      if (.not.associated(next)) then
         if (present(stat)) stat = toml_stat%fatal
         if (present(origin)) origin = current%origin
         return
      end if
      current => next
   end do
   ptr => current
end subroutine walk_path


end module tomlf_build_path
