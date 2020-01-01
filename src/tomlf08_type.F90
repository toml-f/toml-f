! This file is part of toml-f.
!
! Copyright (C) 2019-2020 Sebastian Ehlert
!
! toml-f is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! toml-f is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with toml-f.  If not, see <https://www.gnu.org/licenses/>.

#:set toml_types = ("keyval", "array", "table")
#:set toml_float_kinds = ("real32", "real64")
#:set toml_integer_kinds = ("int16", "int32", "int64")
#:set toml_bool_kinds = toml_integer_kinds
#:set toml_timestamp_kinds = ("date", "time", "datetime")
!> Constants and type definitions for the TOML serializer and deserializer.
module tomlf08_type
   use tomlf08_constants
   use iso_fortran_env
   implicit none
   private

   public :: toml_value
   #:for toml_type in toml_types
   public :: toml_${toml_type}$
   #:endfor
   public :: toml_visitor

   !> Abstact TOML value.
   type, abstract :: toml_value
      !> Key to this value.
      character(len=:), allocatable :: key
   contains
      !> Deconstructor to deallocate this TOML value.
      procedure(deconstructor), deferred :: destroy
      !> Accept a visitor to transverse the data structure.
      procedure(acceptor), deferred :: accept
   end type

   !> TOML key-value pair.
   type, extends(toml_value) :: toml_keyval
      !> The raw value.
      character(len=:), allocatable :: val
   contains
      generic :: get_value => get_string
      !> Convert raw value to real.
      procedure, private :: get_string => keyval_get_string
      #:for vtype in toml_float_kinds
      generic :: get_value => get_float_${vtype}$
      !> Convert raw value to real.
      procedure, private :: get_float_${vtype}$ => keyval_get_float_${vtype}$
      #:endfor
      #:for vtype in toml_integer_kinds
      generic :: get_value => get_integer_${vtype}$
      !> Convert raw value to integer.
      procedure, private :: get_integer_${vtype}$ => keyval_get_integer_${vtype}$
      #:endfor
      #:for vtype in toml_bool_kinds
      generic :: get_value => get_bool_${vtype}$
      !> Convert raw value to logical.
      procedure, private :: get_bool_${vtype}$ => keyval_get_bool_${vtype}$
      #:endfor
      #:for vtype in toml_timestamp_kinds
      generic :: get_value => get_timestamp_${vtype}$
      !> Convert raw value to time stamp
      procedure, private :: get_timestamp_${vtype}$ => keyval_get_timestamp_${vtype}$
      #:endfor
      !> Deconstructor to free the TOML key-value pair.
      procedure :: destroy => keyval_destroy
      !> Accept a visitor to transverse the data structure.
      procedure :: accept => keyval_accept
   end type toml_keyval

   !> TOML array
   type, extends(toml_value) :: toml_array
      !> element kind
      integer(toml_kind) :: kind = INVALID_KIND
      !> for value kind
      integer(toml_type) :: type = INVALID_TYPE
      !> number of elements
      integer :: nelem = 0
      !> polymorphic array
      class(toml_value), allocatable :: elem(:)
   contains
      !> Deconstructor to deallocate the TOML array.
      procedure :: destroy => array_destroy
      #:for toml_type in toml_types
      !> Allocate TOML array instance for ${toml_type}$ content.
      procedure :: new_${toml_type}$ => array_new_${toml_type}$
      !> Append ${toml_type}$ to this array, fails for type mismatch.
      procedure :: push_back_${toml_type}$ => array_push_back_${toml_type}$
      #:endfor
      !> Generic wrapper to append a TOML value to this array.
      procedure :: push_back => array_push_back
      !> Return type of the elements.
      procedure :: get_kind => array_get_kind
      !> Return value kind for in case of value elements.
      procedure :: get_type => array_get_type
      !> Accept a visitor to transverse the data structure.
      procedure :: accept => array_accept
   end type toml_array

   !> TOML table.
   type, extends(toml_value) :: toml_table
      !> Table was created implicitly.
      logical :: implicit = .false.
      #:for toml_type in toml_types
      !> Number of ${toml_type}$s in this table.
      integer :: n${toml_type}$ = 0
      !> Stack for ${toml_type}$s in this table.
      type(toml_${toml_type}$), allocatable :: ${toml_type}$(:)
      #:endfor
   contains
      !> Deconstructor for the TOML table instance.
      procedure :: destroy => table_destroy
      !> Check if key is already present in this table instance.
      procedure :: has_key => table_has_key
      #:for toml_type in toml_types
      !> Interface to append to table.
      generic :: push_back => push_back_${toml_type}$
      !> Append ${toml_type}$ to table (checks automatically for key).
      procedure, private :: push_back_${toml_type}$ => table_push_back_${toml_type}$
      !> Inteface to get ${toml_type}$ at given key.
      generic :: get_${toml_type}$ => get_${toml_type}$_pos, get_${toml_type}$_ptr
      !> Gets position of ${toml_type}$ at given key.
      procedure, private :: get_${toml_type}$_pos => table_get_${toml_type}$_pos
      !> Creates a pointer to ${toml_type}$ at given key.
      procedure, private :: get_${toml_type}$_ptr => table_get_${toml_type}$_ptr
      #:endfor
      !> Accept a visitor to transverse the data structure.
      procedure :: accept => table_accept
   end type toml_table

   #:for toml_type in toml_types
   !> Constructor interface for ${toml_type}.
   interface toml_${toml_type}$
      module procedure :: new_${toml_type}$
   end interface
   #:endfor

   !> Overloaded interface for TOML types.
   interface resize
      #:for toml_type in toml_types
      module procedure :: resize_${toml_type}$
      #:endfor
   end interface

   type, abstract :: toml_visitor
   contains
      #:for toml_type in toml_types
      generic :: visit => visit_${toml_type}$
      procedure(visitor_visit_${toml_type}$), deferred :: visit_${toml_type}$
      #:endfor
   end type toml_visitor

   abstract interface
   #:for toml_type in toml_types
   recursive subroutine visitor_visit_${toml_type}$(visitor, ${toml_type}$)
   import toml_visitor, toml_${toml_type}$
   class(toml_visitor), intent(inout) :: visitor
   class(toml_${toml_type}$), intent(in) :: ${toml_type}$
   end subroutine visitor_visit_${toml_type}$
   #:endfor
   !> Binding for the deconstructor interface, might be recursive.
   recursive subroutine deconstructor(self)
      import toml_value
      !> Instance of the TOML value to deallocate.
      class(toml_value), intent(inout) :: self
   end subroutine
   !> Accept a visitor to transverse the data structure.
   recursive subroutine acceptor(self, visitor)
      import toml_value, toml_visitor
      !> Instance of the TOML value.
      class(toml_value), intent(in) :: self
      !> Visitor for this value.
      class(toml_visitor), intent(inout) :: visitor
   end subroutine
   end interface

contains

#:for toml_type in toml_types
subroutine ${toml_type}$_accept(self, visitor)
   class(toml_${toml_type}$), intent(in) :: self
   class(toml_visitor), intent(inout) :: visitor
   call visitor%visit(self)
end subroutine ${toml_type}$_accept
#:endfor

#:for toml_type in toml_types
pure subroutine resize_${toml_type}$(self, n)
   !> Stack of ${toml_type}$s.
   type(toml_${toml_type}$), allocatable, intent(inout) :: self(:)
   integer, intent(in), optional :: n
   type(toml_${toml_type}$), allocatable :: tmp(:)
   integer :: this_size, new_size
   if (allocated(self)) then
      this_size = size(self, 1)
      call move_alloc(self, tmp)
   else
      this_size = 6
   end if

   if (present(n)) then
      new_size = n
   else
      new_size = this_size + this_size/2 + 1
   end if

   allocate(self(new_size))

   if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(self, 1))
      self(:this_size) = tmp(:this_size)
      deallocate(tmp)
   end if
end subroutine resize_${toml_type}$
#:endfor

subroutine resize_gen(self, n)
   class(toml_value), allocatable, intent(inout) :: self(:)
   integer, intent(in), optional :: n
   class(toml_value), allocatable :: tmp(:)
   integer :: this_size, new_size
   if (allocated(self)) then
      this_size = size(self, 1)
      call move_alloc(self, tmp)

      if (present(n)) then
         new_size = n
      else
         new_size = this_size + this_size/2 + 1
      end if

      ! reallocate array with its old image as mold, to ensure the type is correct
      allocate(self(new_size), mold=tmp)

      this_size = min(size(tmp, 1), size(self, 1))

      ! we know the type, but we still have to convince the compiler that we now
      select type(self)
      #:for toml_type in toml_types
      type is (toml_${toml_type}$)
         select type(tmp)
         type is (toml_${toml_type}$); self(:this_size) = tmp(:this_size)
         end select
      #:endfor
      end select
      deallocate(tmp)
   end if
end subroutine resize_gen

#:for toml_type in toml_types
!> Find position of table at a given key.
subroutine table_get_${toml_type}$_pos(self, key, pos)
   !> Root TOML table.
   class(toml_table), intent(in) :: self
   !> Key of table.
   character(len=*), intent(in) :: key
   !> Pointer to table at given key.
   integer, intent(out) :: pos
   integer :: i
   pos = 0
   do concurrent(i = 1:self%n${toml_type}$)
      if (self%${toml_type}$(i)%key == key) pos = i
   end do
end subroutine table_get_${toml_type}$_pos

!> Return a pointer to the table at key in a TOML table.
subroutine table_get_${toml_type}$_ptr(self, key, ptr)
   !> Root TOML table.
   class(toml_table), intent(in), target :: self
   !> Key of table.
   character(len=*), intent(in) :: key
   !> Pointer to table at given key.
   type(toml_${toml_type}$), pointer, intent(out) :: ptr
   integer :: pos
   nullify(ptr)
   call table_get_${toml_type}$_pos(self, key, pos)
   if (pos > 0) ptr => self%${toml_type}$(pos)
end subroutine table_get_${toml_type}$_ptr
#:endfor

logical function table_has_key(self, key) result(has_key)
   class(toml_table), intent(in) :: self
   character(len=*), intent(in) :: key
   integer :: pos
   has_key = .false.
   #:for toml_type in toml_types
   call self%get_${toml_type}$(key, pos)
   has_key = pos > 0
   if (has_key) return
   #:endfor
end function table_has_key

#:for toml_type in toml_types
subroutine table_push_back_${toml_type}$(self, key, ptr, stat)
   class(toml_table), intent(inout), target :: self
   character(len=*), intent(in) :: key
   type(toml_${toml_type}$), pointer, intent(out) :: ptr
   integer, intent(out), optional :: stat
   nullify(ptr)
   #:if (toml_type == "table")
   !> tables can already created implicitly, if it is, we make it explicit
   call self%get_table(key, ptr)
   if (associated(ptr)) then
      if (ptr%implicit) then
         ptr%implicit = .false.
         if (present(stat)) stat = 0
      else
         if (present(stat)) stat = 1
      end if
      return
   endif
   #:else
   if (self%has_key(key)) then
      if (present(stat)) stat = 1
      return
   endif
   #:endif
   if (allocated(self%${toml_type}$)) then
      if (self%n${toml_type}$ >= size(self%${toml_type}$, 1)) then
         call resize(self%${toml_type}$)
      end if
   else
      call resize(self%${toml_type}$)
   end if
   self%n${toml_type}$ = self%n${toml_type}$ + 1
   self%${toml_type}$(self%n${toml_type}$)%key = key
   ptr => self%${toml_type}$(self%n${toml_type}$)
   if (present(stat)) stat = 0
end subroutine table_push_back_${toml_type}$
#:endfor

#:for toml_type in toml_types
!> Allocate a new TOML array for ${toml_type}$ elements.
subroutine array_new_${toml_type}$(self, size, stat)
   !> TOML array instance.
   class(toml_array), intent(inout) :: self
   !> Size of the initial TOML array.
   integer, intent(in), optional :: size
   !> Status of the constructor.
   integer, intent(out), optional :: stat
   if (allocated(self%elem)) then
      if (present(stat)) stat = 1
      return
   end if
   if (present(size)) then
      allocate(toml_${toml_type}$ :: self%elem(size))
   else
      allocate(toml_${toml_type}$ :: self%elem(10))
   end if
   self%nelem = 0
   self%kind = ${toml_type}$_KIND
   self%type = INVALID_TYPE
   if (present(stat)) stat = 0
end subroutine array_new_${toml_type}$
#:endfor

integer(toml_kind) function array_get_kind(self) result(akind)
   class(toml_array), intent(in) :: self
   akind = self%kind
end function array_get_kind

integer(toml_type) function array_get_type(self) result(atype)
   class(toml_array), intent(in) :: self
   if (self%kind /= KEYVAL_KIND) then
      atype = INVALID_TYPE
   else if (self%nelem == 0) then
      atype = INVALID_TYPE
   else
      atype = self%type
   end if
end function array_get_type

subroutine array_push_back(self, ptr, stat)
   class(toml_array), intent(inout), target :: self
   class(toml_value), pointer, intent(out) :: ptr
   integer, intent(out), optional :: stat
   nullify(ptr)
   if (.not.allocated(self%elem)) then
      if (present(stat)) stat = 1
      return
   end if
   if (self%nelem >= size(self%elem, 1)) call resize_gen(self%elem)
   self%nelem = self%nelem + 1
   ptr => self%elem(self%nelem)
   if (present(stat)) stat = 0
end subroutine array_push_back

#:for toml_type in toml_types
subroutine array_push_back_${toml_type}$(self, ptr, stat)
   class(toml_array), intent(inout), target :: self
   type(toml_${toml_type}$), pointer, intent(out) :: ptr
   integer, intent(out), optional :: stat
   nullify(ptr)
   if (.not.allocated(self%elem)) then
      if (present(stat)) stat = 1
      return
   end if
   if (self%nelem >= size(self%elem, 1)) call resize_gen(self%elem)
   self%nelem = self%nelem + 1
   select type(elem => self%elem(self%nelem))
   type is(toml_${toml_type}$)
      ptr => elem
      if (present(stat)) stat = 0
   class default
      if (present(stat)) stat = 1
   end select
end subroutine array_push_back_${toml_type}$
#:endfor

!> Deconstructor for TOML key-value.
subroutine keyval_destroy(self)
   !> TOML key-value instance.
   class(toml_keyval), intent(inout) :: self
   if (allocated(self%key)) deallocate(self%key)
   if (allocated(self%val)) deallocate(self%val)
end subroutine

!> Deconstructor for TOML arrays.
recursive subroutine array_destroy(self)
   !> TOML array instance.
   class(toml_array), intent(inout) :: self
   integer :: ii
   if (allocated(self%key)) deallocate(self%key)
   do ii = 1, self%nelem
      call self%elem(ii)%destroy
   end do
   self%kind = INVALID_KIND
   self%type = INVALID_KIND
   self%nelem = 0
   if (allocated(self%elem)) deallocate(self%elem)
end subroutine array_destroy

!> Deconstructor for TOML table.
recursive subroutine table_destroy(self)
   !> TOML table instance.
   class(toml_table), intent(inout) :: self
   integer :: ii
   if (allocated(self%key)) deallocate(self%key)
   self%implicit = .false.
   #:for toml_type in toml_types
   if (allocated(self%${toml_type}$)) then
      do ii = 1, self%n${toml_type}$
         call self%${toml_type}$(ii)%destroy
      end do
      self%n${toml_type}$ = 0
      deallocate(self%${toml_type}$)
   end if
   #:endfor
end subroutine table_destroy

#:for vtype in toml_float_kinds
subroutine keyval_get_float_${vtype}$(self, value, status)
   use tomlf08_utils
   class(toml_keyval), intent(in) :: self
   real(${vtype}$), intent(out) :: value
   logical, intent(out), optional :: status
   logical :: stat
   real(TOML_FLOAT_KIND) :: dummy
   stat = toml_raw_to_float(self%val, dummy)
   if (stat) value = real(dummy, ${vtype}$)
   if (present(status)) status = stat
end subroutine keyval_get_float_${vtype}$
#:endfor

#:for vtype in toml_integer_kinds
subroutine keyval_get_integer_${vtype}$(self, value, status)
   use tomlf08_utils
   class(toml_keyval), intent(in) :: self
   integer(${vtype}$), intent(out) :: value
   logical, intent(out), optional :: status
   logical :: stat
   integer(TOML_INTEGER_KIND) :: dummy
   stat = toml_raw_to_integer(self%val, dummy)
   if (stat) value = int(dummy, ${vtype}$)
   if (present(status)) status = stat
end subroutine keyval_get_integer_${vtype}$
#:endfor

#:for vtype in toml_bool_kinds
subroutine keyval_get_bool_${vtype}$(self, value, status)
   use tomlf08_utils
   class(toml_keyval), intent(in) :: self
   logical(${vtype}$), intent(out) :: value
   logical, intent(out), optional :: status
   logical :: stat
   logical(TOML_BOOL_KIND) :: dummy
   stat = toml_raw_to_bool(self%val, dummy)
   if (stat) value = dummy
   if (present(status)) status = stat
end subroutine keyval_get_bool_${vtype}$
#:endfor

#:for vtype in toml_timestamp_kinds
subroutine keyval_get_timestamp_${vtype}$(self, value, status)
   use tomlf08_utils
   class(toml_keyval), intent(in) :: self
   type(toml_${vtype}$), intent(out) :: value
   logical, intent(out), optional :: status
   logical :: stat
   type(toml_datetime) :: dummy
   stat = toml_raw_to_timestamp(self%val, dummy)
   #:if (vtype == "datetime")
   if (stat) value = dummy
   #:else
   stat = stat .and. allocated(dummy%${vtype}$)
   if (stat) value = dummy%${vtype}$
   #:endif
   if (present(status)) status = stat
end subroutine keyval_get_timestamp_${vtype}$
#:endfor

subroutine keyval_get_string(self, value, status)
   use tomlf08_utils
   class(toml_keyval), intent(in) :: self
   character(len=:), allocatable, intent(out) :: value
   logical, intent(out), optional :: status
   logical :: stat
   stat = toml_raw_verify_string(self%val)
   if (stat) value = self%val
   if (present(status)) status = stat
end subroutine keyval_get_string

type(toml_keyval) function new_keyval(key, raw)
   character(len=*), intent(in), optional :: key
   character(len=*), intent(in) :: raw
   if (present(key)) new_keyval%key = key
   new_keyval%val = raw
end function new_keyval

type(toml_table) function new_table(key, values, tables, arrays)
   character(len=*), intent(in), optional :: key
   type(toml_keyval), intent(in), optional :: values(:)
   type(toml_table), intent(in), optional :: tables(:)
   type(toml_array), intent(in), optional :: arrays(:)
   if (present(key)) new_table%key = key
   if (present(values)) then
      new_table%nkeyval = size(values, 1)
      new_table%keyval = values
   end if
   if (present(arrays)) then
      new_table%narray = size(arrays, 1)
      new_table%array = arrays
   end if
   if (present(tables)) then
      new_table%ntable = size(tables, 1)
      new_table%table = tables
   end if
end function new_table

type(toml_array) function new_array(key, values)
   use tomlf08_utils
   character(len=*), intent(in), optional :: key
   class(toml_value), intent(in), optional :: values(:)
   if (present(key)) new_array%key = key
   select type(values)
   #:for toml_type in toml_types
   class is(toml_${toml_type}$)
      new_array%kind = ${toml_type}$_KIND
      #:if (toml_type == "keyval")
      new_array%type = toml_get_value_type(values(1)%val)
      #:endif
      new_array%nelem = size(values, 1)
   #:endfor
   end select
   new_array%elem = values
end function new_array

end module tomlf08_type
