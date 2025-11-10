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

!> TOML key-value pair
module tomlf_type_keyval
   use tomlf_constants, only : tfc, tfr, tfi, toml_type
   use tomlf_datetime, only : toml_datetime
   use tomlf_type_value, only : toml_value, toml_visitor
   implicit none
   private

   public :: toml_keyval, new_keyval, new


   !> Generic TOML value
   type, abstract :: generic_value
   end type generic_value

   !> TOML real value
   type, extends(generic_value) :: float_value
      real(tfr) :: raw
   end type float_value

   !> TOML integer value
   type, extends(generic_value) :: integer_value
      integer(tfi) :: raw
   end type integer_value

   !> TOML boolean value
   type, extends(generic_value) :: boolean_value
      logical :: raw
   end type boolean_value

   !> TOML datetime value
   type, extends(generic_value) :: datetime_value
      type(toml_datetime) :: raw
   end type datetime_value

   !> TOML string value
   type, extends(generic_value) :: string_value
      character(:, tfc), allocatable :: raw
   end type string_value
      


   !> TOML key-value pair
   type, extends(toml_value) :: toml_keyval

      !> Actual TOML value
      class(generic_value), allocatable :: val

      !> Origin of value
      integer :: origin_value = 0

   contains

      !> Get the value stored in the key-value pair
      generic :: get => get_float, get_integer, get_boolean, get_datetime, get_string
      procedure :: get_float
      procedure :: get_integer
      procedure :: get_boolean
      procedure :: get_datetime
      procedure :: get_string

      !> Set the value for the key-value pair
      generic :: set => set_float, set_integer, set_boolean, set_datetime, set_string
      procedure :: set_float
      procedure :: set_integer
      procedure :: set_boolean
      procedure :: set_datetime
      procedure :: set_string

      !> Get the type of the value stored in the key-value pair
      procedure :: get_type

      !> Release allocation hold by TOML key-value pair
      procedure :: destroy

   end type toml_keyval


   !> Overloaded constructor for TOML values
   interface new
      module procedure :: new_keyval
   end interface


contains


!> Constructor to create a new TOML key-value pair
subroutine new_keyval(self)

   !> Instance of the TOML key-value pair
   type(toml_keyval), intent(out) :: self

   associate(self => self); end associate

end subroutine new_keyval


!> Deconstructor to cleanup allocations (optional)
subroutine destroy(self)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%val)) then
      deallocate(self%val)
   end if

end subroutine destroy


!> Obtain real value from TOML key-value pair
subroutine get_float(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value to be assigned
   real(tfr), pointer, intent(out) :: val

   val => cast_float(self%val)
end subroutine get_float


!> Obtain integer value from TOML key-value pair
subroutine get_integer(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value to be assigned
   integer(tfi), pointer, intent(out) :: val

   val => cast_integer(self%val)
end subroutine get_integer


!> Obtain boolean value from TOML key-value pair
subroutine get_boolean(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value to be assigned
   logical, pointer, intent(out) :: val

   val => cast_boolean(self%val)
end subroutine get_boolean


!> Obtain datetime value from TOML key-value pair
subroutine get_datetime(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value to be assigned
   type(toml_datetime), pointer, intent(out) :: val

   val => cast_datetime(self%val)
end subroutine get_datetime


!> Obtain datetime value from TOML key-value pair
subroutine get_string(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value to be assigned
   character(:, tfc), pointer, intent(out) :: val

   val => cast_string(self%val)
end subroutine get_string


!> Obtain real value from TOML key-value pair
subroutine set_float(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Value to be assigned
   real(tfr), intent(in) :: val

   type(float_value), allocatable :: tmp

   allocate(tmp)
   tmp%raw = val
   call move_alloc(tmp, self%val)
end subroutine set_float


!> Obtain integer value from TOML key-value pair
subroutine set_integer(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Value to be assigned
   integer(tfi), intent(in) :: val

   type(integer_value), allocatable :: tmp

   allocate(tmp)
   tmp%raw = val
   call move_alloc(tmp, self%val)
end subroutine set_integer


!> Obtain boolean value from TOML key-value pair
subroutine set_boolean(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Value to be assigned
   logical, intent(in) :: val

   type(boolean_value), allocatable :: tmp

   allocate(tmp)
   tmp%raw = val
   call move_alloc(tmp, self%val)
end subroutine set_boolean


!> Obtain datetime value from TOML key-value pair
subroutine set_datetime(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Value to be assigned
   type(toml_datetime), intent(in) :: val

   type(datetime_value), allocatable :: tmp

   allocate(tmp)
   tmp%raw = val
   call move_alloc(tmp, self%val)
end subroutine set_datetime


!> Obtain datetime value from TOML key-value pair
subroutine set_string(self, val)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Value to be assigned
   character(*, tfc), intent(in) :: val

   type(string_value), allocatable :: tmp

   allocate(tmp)
   tmp%raw = val
   call move_alloc(tmp, self%val)
end subroutine set_string


!> Get the type of the value stored in the key-value pair
pure function get_type(self) result(value_type)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(in) :: self

   !> Value type
   integer :: value_type

   select type(val => self%val)
   class default
      value_type = toml_type%invalid
   type is(float_value)
      value_type = toml_type%float
   type is(integer_value)
      value_type = toml_type%int
   type is(boolean_value)
      value_type = toml_type%boolean
   type is(datetime_value)
      value_type = toml_type%datetime
   type is(string_value)
      value_type = toml_type%string
   end select
end function get_type


function cast_float(val) result(ptr)
   class(generic_value), intent(in), target :: val
   real(tfr), pointer :: ptr

   nullify(ptr)
   select type(val)
   type is(float_value)
      ptr => val%raw
   end select
end function cast_float

function cast_integer(val) result(ptr)
   class(generic_value), intent(in), target :: val
   integer(tfi), pointer :: ptr

   nullify(ptr)
   select type(val)
   type is(integer_value)
      ptr => val%raw
   end select
end function cast_integer

function cast_boolean(val) result(ptr)
   class(generic_value), intent(in), target :: val
   logical, pointer :: ptr

   nullify(ptr)
   select type(val)
   type is(boolean_value)
      ptr => val%raw
   end select
end function cast_boolean

function cast_datetime(val) result(ptr)
   class(generic_value), intent(in), target :: val
   type(toml_datetime), pointer :: ptr

   nullify(ptr)
   select type(val)
   type is(datetime_value)
      ptr => val%raw
   end select
end function cast_datetime

function cast_string(val) result(ptr)
   class(generic_value), intent(in), target :: val
   character(:, tfc), pointer :: ptr

   nullify(ptr)
   select type(val)
   type is(string_value)
      ptr => val%raw
   end select
end function cast_string

end module tomlf_type_keyval
