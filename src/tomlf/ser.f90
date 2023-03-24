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

!> TOML serializer implementation
module tomlf_ser
   use tomlf_constants, only : tfc, tfi, tfr, tfout, toml_type
   use tomlf_datetime, only : toml_datetime, to_string
   use tomlf_error, only : toml_error, toml_stat, make_error
   use tomlf_type, only : toml_value, toml_visitor, toml_key, toml_table, &
      & toml_array, toml_keyval, is_array_of_tables, len
   use tomlf_utils, only : to_string, toml_escape_string
   implicit none
   private

   public :: toml_serializer, new_serializer, new
   public :: toml_dump, toml_dumps, toml_serialize


   interface toml_dumps
      module procedure :: toml_dump_to_string
   end interface toml_dumps

   interface toml_dump
      module procedure :: toml_dump_to_file
      module procedure :: toml_dump_to_unit
   end interface toml_dump


   !> Configuration for JSON serializer
   type :: toml_ser_config

      !> Indentation
      character(len=:), allocatable :: indent

   end type toml_ser_config


   !> TOML serializer to produduce a TOML document from a datastructure
   type, extends(toml_visitor) :: toml_serializer
      private

      !> Output string
      character(:), allocatable :: output

      !> Configuration for serializer
      type(toml_ser_config) :: config = toml_ser_config()

      !> Special mode for printing array of tables
      logical, private :: array_of_tables = .false.

      !> Special mode for printing inline arrays
      logical, private :: inline_array = .false.

      !> Top of the key stack
      integer, private :: top = 0

      !> Key stack to create table headers
      type(toml_key), allocatable, private :: stack(:)

   contains

      !> Visit a TOML value
      procedure :: visit

   end type toml_serializer


   !> Create standard constructor
   interface toml_serializer
      module procedure :: new_serializer_func
   end interface toml_serializer


   !> Overloaded constructor for TOML serializers
   interface new
      module procedure :: new_serializer
   end interface


   !> Initial size of the key path stack
   integer, parameter :: initial_size = 8


contains


!> Serialize a JSON value to a string and return it.
!>
!> In case of an error this function will invoke an error stop.
function toml_serialize(val, config) result(string)
   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   !> Serialized JSON value
   character(len=:), allocatable :: string

   type(toml_error), allocatable :: error

   call toml_dumps(val, string, error, config=config)
   if (allocated(error)) then
      error stop error%message
   end if
end function toml_serialize


!> Create a string representing the JSON value
subroutine toml_dump_to_string(val, string, error, config)

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> Formatted unit to write to
   character(:), allocatable, intent(out) :: string

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   type(toml_serializer) :: ser

   ser = toml_serializer(config=config)
   call val%accept(ser)
   string = ser%output
end subroutine toml_dump_to_string


!> Write string representation of JSON value to a connected formatted unit
subroutine toml_dump_to_unit(val, io, error, config)

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> Formatted unit to write to
   integer, intent(in) :: io

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   character(len=:), allocatable :: string
   character(512) :: msg
   integer :: stat

   call toml_dumps(val, string, error, config=config)
   if (allocated(error)) return
   write(io, '(a)', iostat=stat, iomsg=msg) string
   if (stat /= 0) then
      call make_error(error, trim(msg))
      return
   end if
end subroutine toml_dump_to_unit


!> Write string representation of JSON value to a file
subroutine toml_dump_to_file(val, filename, error, config)

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   !> File name to write to
   character(*), intent(in) :: filename

   !> Error handling
   type(toml_error), allocatable, intent(out) :: error

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   integer :: io
   integer :: stat
   character(512) :: msg

   open(file=filename, newunit=io, iostat=stat, iomsg=msg)
   if (stat /= 0) then
      call make_error(error, trim(msg))
      return
   end if
   call toml_dump(val, io, error, config=config)
   close(unit=io, iostat=stat, iomsg=msg)
   if (.not.allocated(error) .and. stat /= 0) then
      call make_error(error, trim(msg))
   end if
end subroutine toml_dump_to_file


!> Constructor to create new serializer instance
subroutine new_serializer(self, config)

   !> Instance of the TOML serializer
   type(toml_serializer), intent(out) :: self

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   self%output = ""
   if (present(config)) self%config = config
end subroutine new_serializer


!> Default constructor for TOML serializer
function new_serializer_func(config) result(self)

   !> Configuration for serializer
   type(toml_ser_config), intent(in), optional :: config

   !> Instance of the TOML serializer
   type(toml_serializer) :: self

   call new_serializer(self, config)
end function new_serializer_func


!> Visit a TOML value
recursive subroutine visit(self, val)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: self

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   select type(val)
   class is(toml_keyval)
      call visit_keyval(self, val)
   class is(toml_array)
      call visit_array(self, val)
   class is(toml_table)
      call visit_table(self, val)
   end select

end subroutine visit


!> Visit a TOML key-value pair
subroutine visit_keyval(visitor, keyval)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_keyval), intent(inout) :: keyval

   character(kind=tfc, len=:), allocatable :: key, str
   type(toml_datetime), pointer :: dval
   character(:, tfc), pointer :: sval
   integer(tfi), pointer :: ival
   real(tfr), pointer :: rval
   logical, pointer :: lval

   call keyval%get_key(key)

   select case(keyval%get_type())
   case(toml_type%string)
      call keyval%get(sval)
      call toml_escape_string(sval, str)
   case(toml_type%int)
      call keyval%get(ival)
      str = to_string(ival)
   case(toml_type%float)
      call keyval%get(rval)
      str = to_string(rval)
   case(toml_type%boolean)
      call keyval%get(lval)
      if (lval) then
         str = "true"
      else
         str = "false"
      end if
   case(toml_type%datetime)
      call keyval%get(dval)
      str = to_string(dval)
   end select

   if (visitor%inline_array) then
      visitor%output = visitor%output // " "
   end if
   visitor%output = visitor%output // key // " = " // str
   if (.not.visitor%inline_array) then
      visitor%output = visitor%output // new_line('a')
   end if

end subroutine visit_keyval


!> Visit a TOML array
recursive subroutine visit_array(visitor, array)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_array), intent(inout) :: array

   class(toml_value), pointer :: ptr
   character(kind=tfc, len=:), allocatable :: key, str
   type(toml_datetime), pointer :: dval
   character(:, tfc), pointer :: sval
   integer(tfi), pointer :: ival
   real(tfr), pointer :: rval
   logical, pointer :: lval
   integer :: i, n

   if (visitor%inline_array) visitor%output = visitor%output // " ["
   n = len(array)
   do i = 1, n
      call array%get(i, ptr)
      select type(ptr)
      class is(toml_keyval)

         select case(ptr%get_type())
         case(toml_type%string)
            call ptr%get(sval)
            call toml_escape_string(sval, str)
         case(toml_type%int)
            call ptr%get(ival)
            str = to_string(ival)
         case(toml_type%float)
            call ptr%get(rval)
            str = to_string(rval)
         case(toml_type%boolean)
            call ptr%get(lval)
            if (lval) then
               str = "true"
            else
               str = "false"
            end if
         case(toml_type%datetime)
            call ptr%get(dval)
            str = to_string(dval)
         end select

         visitor%output = visitor%output // " " // str
         if (i /= n) visitor%output = visitor%output // ","
      class is(toml_array)
         call ptr%accept(visitor)
         if (i /= n) visitor%output = visitor%output // ","
      class is(toml_table)
         if (visitor%inline_array) then
            visitor%output = visitor%output // " {"
            call ptr%accept(visitor)
            visitor%output = visitor%output // " }"
            if (i /= n) visitor%output = visitor%output // ","
         else
            visitor%array_of_tables = .true.
            if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
            visitor%top = visitor%top + 1
            call array%get_key(key)
            visitor%stack(visitor%top)%key = key
            call ptr%accept(visitor)
            deallocate(visitor%stack(visitor%top)%key)
            visitor%top = visitor%top - 1
         end if
      end select
   end do
   if (visitor%inline_array) visitor%output = visitor%output // " ]"

end subroutine visit_array


!> Visit a TOML table
recursive subroutine visit_table(visitor, table)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML table to visit
   type(toml_table), intent(inout) :: table

   class(toml_value), pointer :: ptr
   type(toml_key), allocatable :: list(:)
   logical, allocatable :: defer(:)
   character(kind=tfc, len=:), allocatable :: key
   integer :: i, n

   call table%get_keys(list)

   n = size(list, 1)
   allocate(defer(n))

   if (.not.allocated(visitor%stack)) then
      call resize(visitor%stack)
   else
      if (.not.(visitor%inline_array .or. table%implicit)) then
         visitor%output = visitor%output // "["
         if (visitor%array_of_tables) visitor%output = visitor%output // "["
         do i = 1, visitor%top-1
            visitor%output = visitor%output // visitor%stack(i)%key // "."
         end do
         visitor%output = visitor%output // visitor%stack(visitor%top)%key
         visitor%output = visitor%output // "]"
         if (visitor%array_of_tables) visitor%output = visitor%output // "]"
         visitor%output = visitor%output // new_line('a')
         visitor%array_of_tables = .false.
      end if
   end if

   do i = 1, n
      defer(i) = .false.
      call table%get(list(i)%key, ptr)
      select type(ptr)
      class is(toml_keyval)
         call ptr%accept(visitor)
         if (visitor%inline_array) then
            if (i /= n) visitor%output = visitor%output // ","
         end if
      class is(toml_array)
         if (visitor%inline_array) then
            call ptr%get_key(key)
            visitor%output = visitor%output // " " // key // " ="
            call ptr%accept(visitor)
            if (i /= n) visitor%output = visitor%output // ","
         else
            if (is_array_of_tables(ptr)) then
               ! Array of tables open a new section
               ! -> cannot serialize them before all key-value pairs are done
               defer(i) = .true.
            else
               visitor%inline_array = .true.
               call ptr%get_key(key)
               visitor%output = visitor%output // key // " ="
               call ptr%accept(visitor)
               visitor%inline_array = .false.
               visitor%output = visitor%output // new_line('a')
            end if
         end if
      class is(toml_table)
         ! Subtables open a new section
         ! -> cannot serialize them before all key-value pairs are done
         defer(i) = .true.
      end select
   end do

   do i = 1, n
      if (defer(i)) then
         call table%get(list(i)%key, ptr)
         select type(ptr)
         class is(toml_keyval)
            call ptr%accept(visitor)
            if (visitor%inline_array) then
               if (i /= n) visitor%output = visitor%output // ","
            end if
         class is(toml_array)
            if (visitor%inline_array) then
               call ptr%get_key(key)
               visitor%output = visitor%output // " " // key // " ="
               call ptr%accept(visitor)
               if (i /= n) visitor%output = visitor%output // ","
            else
               if (is_array_of_tables(ptr)) then
                  call ptr%accept(visitor)
               else
                  visitor%inline_array = .true.
                  call ptr%get_key(key)
                  visitor%output = visitor%output // key // " ="
                  call ptr%accept(visitor)
                  visitor%inline_array = .false.
                  visitor%output = visitor%output // new_line('a')
               end if
            end if
         class is(toml_table)
            if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
            visitor%top = visitor%top + 1
            call ptr%get_key(key)
            visitor%stack(visitor%top)%key = key
            call ptr%accept(visitor)
            deallocate(visitor%stack(visitor%top)%key)
            visitor%top = visitor%top - 1
         end select
      end if
   end do

   if (.not.visitor%inline_array .and. visitor%top == 0) then
      deallocate(visitor%stack)
   end if

end subroutine visit_table


!> Change size of the stack
subroutine resize(stack, n)

   !> Stack of keys to be resized
   type(toml_key), allocatable, intent(inout) :: stack(:)

   !> New size of the stack
   integer, intent(in), optional :: n

   type(toml_key), allocatable :: tmp(:)
   integer :: m

   if (present(n)) then
      m = n
   else
      if (allocated(stack)) then
         m = size(stack)
         m = m + m/2 + 1
      else
         m = initial_size
      end if
   end if

   if (allocated(stack)) then
      call move_alloc(stack, tmp)
      allocate(stack(m))

      m = min(size(tmp), m)
      stack(:m) = tmp(:m)

      deallocate(tmp)
   else
      allocate(stack(m))
   end if

end subroutine resize


end module tomlf_ser
