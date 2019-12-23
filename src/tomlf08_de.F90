! This file is part of toml-f.
!
! Copyright (C) 2019 Sebastian Ehlert
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

#:def ASSERT(cond)
   #:if not defined('NDEBUG')
   if (.not.(${cond}$)) then
      error stop "${_FILE_}$:${_LINE_}$: assertion ${cond}$ failed"
   end if
   #:endif
#:enddef ASSERT

!> Deserialization module for TOML.
module tomlf08_de
   use tomlf08_type
   use tomlf08_utils
   implicit none
   private
   public :: toml_parse

   interface toml_parse
      module procedure :: toml_parse_unit
      module procedure :: toml_parse_string
   end interface toml_parse

   !> Possible token types in TOML.
   enum, bind(C)
      enumerator :: INVALID, &
         & DOT, COMMA, EQUAL, LBRACE, RBRACE, NEWLINE, LBRACKET, RBRACKET, &
         & LLBRACKET, RRBRACKET, STRING, EOF
   end enum
   !> Kind parameter for token type enums.
   integer, parameter :: tokentype_t = kind(INVALID)

   type :: line_t
      integer :: pos = 0
      integer :: num = 0
      character(len=:), pointer :: ptr => null()
   end type line_t

   type :: error_t
      integer :: lineno
      character(len=:), allocatable :: message
   end type error_t

   interface error_t
      module procedure :: error_with_line
   end interface

   !> TOML token.
   type :: token_t
      sequence
      integer(tokentype_t) :: tok = INVALID
      character(len=:), pointer :: ptr => null()
      integer :: len = 0
   end type token_t

   !> TOML builder
   type :: toml_deserializer_t
      character(len=:), pointer :: conf
      type(line_t) :: line
      integer :: len
      integer :: lineno

      type(token_t) :: tok
      type(toml_table_t), allocatable :: root
      type(toml_table_t), pointer :: curtab => null()

      type(error_t), allocatable :: error
   end type

contains

!> Parse a TOML input from a given IO unit.
subroutine toml_parse_unit(table, unit, iostat)
   use iso_fortran_env
   type(toml_table_t), allocatable, intent(out) :: table
   integer, intent(in) :: unit
   integer, intent(out), optional :: iostat
   character(len=:), allocatable :: conf
   integer, parameter :: bufsize = 512
   character(len=bufsize) :: buffer
   integer :: size
   integer :: error
   allocate(character(len=0) :: conf)
   do 
      read(unit, '(a)', advance='no', iostat=error, size=size) buffer
      if (error > 0) exit
      conf = conf // buffer(:size)
      if (error < 0) then
         if (is_iostat_eor(error)) then
            error = 0
            conf = conf // TOML_NEWLINE
         end if
         if (is_iostat_end(error)) then
            error = 0
            exit
         end if
      end if
   end do

   if (error /= 0) then
      if (present(iostat)) iostat = error
      return
   end if

   call toml_parse_string(table, conf)

end subroutine toml_parse_unit

subroutine toml_parse_string(table, conf)
   use iso_fortran_env, only: error_unit
   type(toml_table_t), allocatable, intent(out) :: table
   character(len=*), intent(in), target :: conf
   character(len=:), pointer :: ptr
   type(toml_deserializer_t), target :: de

   !> connect deserializer to configuration
   de%line%ptr => conf
   de%line%num = 1
   ptr => conf

   !> first token is an artifical newline
   de%tok = new_token(NEWLINE, ptr, 0)

   !> create a new table
   allocate(de%root)
   de%curtab => de%root

   do while(de%tok%tok /= EOF)
      select case(de%tok%tok)
      case(NEWLINE)
         call next_token(de, .true.)
      case(STRING)
         call parse_keyval(de, de%curtab)
         if (allocated(de%error)) exit
         if (de%tok%tok /=  NEWLINE) then
            de%error = syntax_error(de%line, "extra chars after value")
         end if
         call eat_token(de, NEWLINE, .true.)
      case(LBRACKET)
         call parse_select(de)
         if (allocated(de%error)) exit
      case(LLBRACKET)
         call parse_select(de)
         if (allocated(de%error)) exit
      case default
         de%error = syntax_error(de%line, "syntax error")
         exit
      end select
   end do

   if (allocated(de%error)) then
      write(error_unit, '("line",1x,i0,":",1x,a)') &
         &  de%error%lineno, de%error%message
      return
   end if

   call move_alloc(de%root, table)

end subroutine toml_parse_string

subroutine parse_select(de)
   type :: tablenode_t
      character(len=:), allocatable :: key
      type(token_t) :: tok = token_t()
   end type
   type :: tablepath_t
      integer :: top = 0
      type(tablenode_t) :: node(10) = tablenode_t()
   end type tablepath_t
   type(toml_deserializer_t), intent(inout) :: de
   logical :: llb
   type(token_t) :: top
   character(len=:), allocatable :: key
   type(toml_table_t), pointer :: table
   type(toml_array_t), pointer :: array
   type(tablepath_t) :: path

   llb = de%tok%tok == LLBRACKET

   if (llb) then
      call eat_token(de, LLBRACKET, .true.)
   else
      call eat_token(de, LBRACKET, .true.)
   end if

   call fill_tablepath(de, path)
   if (allocated(de%error)) return

   ! remove topmost element from path
   top = path%node(path%top)%tok
   call move_alloc(path%node(path%top)%key, key)
   path%top = path%top - 1

   call walk_tablepath(de, path)
   if (allocated(de%error)) return

   if (llb) then
      ! [[key.key.top]]
      call de%curtab%get_array(key, array)
      if (.not.associated(array)) then
         call de%curtab%push_back(key, array)
         @:ASSERT(associated(array))
         call array%new_table
      end if
      if (array%get_kind() /= TABLE_KIND) then
         de%error = syntax_error(de%line, "array mismatch")
         return
      end if

      call array%push_back_table(table)
   else
      ! [key.key.top]
      call de%curtab%push_back(key, table)
   end if
   de%curtab => table

   if (llb) then
      if (de%tok%tok /= RRBRACKET) then
         de%error = syntax_error(de%line, "expects ]]")
         return
      end if
      call eat_token(de, RRBRACKET, .true.)
   else
      if (de%tok%tok /= RBRACKET) then
         de%error = syntax_error(de%line, "expects ]")
         return
      end if
      call eat_token(de, RBRACKET, .true.)
   end if

   if (de%tok%tok /= NEWLINE) then
      de%error = syntax_error(de%line, "extra chars after ] or ]]")
      return
   end if

contains

subroutine walk_tablepath(de, path)
   type(toml_deserializer_t), intent(inout), target :: de
   type(tablepath_t), intent(inout), target :: path
   type(toml_table_t), pointer :: table, ptr
   type(toml_array_t), pointer :: array
   character(len=:), pointer :: key
   integer :: i
   table => de%root
   do i = 1, path%top
      key => path%node(i)%key
      if (table%has_key(key)) then
         call table%get_table(key, ptr)
         if (.not.associated(ptr)) then
            call table%get_array(key, array)
            if (.not.associated(array)) then
               de%error = key_exists_error(de%line, key)
               return
            end if
            @:ASSERT(array%get_kind() == TABLE_KIND)
            @:ASSERT(array%nelem > 0)
            select type(elem => array%elem(array%nelem))
            type is(toml_table_t); ptr => elem
            end select
         end if
      else
         call table%push_back(key, ptr)
         ptr%implicit = .true.
      end if
      table => ptr
   end do
   de%curtab => table
end subroutine walk_tablepath

subroutine fill_tablepath(de, path)
   type(toml_deserializer_t), intent(inout) :: de
   type(tablepath_t), intent(inout) :: path
   !> clear path
   path = tablepath_t()
   do
      if (path%top >= 10) then
         de%error = vendor_error(de%line, "table path is too deep, max. allowed is 10.")
      end if

      if (de%tok%tok /= STRING) then
         de%error = syntax_error(de%line, "invalid or missing key")
         return
      end if

      path%top = path%top + 1
      path%node(path%top)%tok = de%tok
      call key_from_token(path%node(path%top)%key, de%tok)
      if (.not.allocated(path%node(path%top)%key)) then
         de%error = syntax_error(de%line, "invalid key")
         return
      end if

      call eat_token(de, STRING, .true.)

      if (de%tok%tok == RBRACKET .or. de%tok%tok == RRBRACKET) exit

      if (de%tok%tok /= DOT) then
         de%error = syntax_error(de%line, "invalid key")
         return
      end if

      call eat_token(de, DOT, .true.)
   end do

   if (path%top <= 0) then
      de%error = syntax_error(de%line, "empty table selector")
   end if

end subroutine fill_tablepath

end subroutine parse_select

recursive subroutine parse_keyval(de, table)
   type(toml_deserializer_t), intent(inout) :: de
   type(toml_table_t), intent(inout) :: table
   type(token_t) :: key
   type(toml_keyval_t), pointer :: vptr
   type(toml_array_t), pointer :: aptr
   type(toml_table_t), pointer :: tptr
   character(len=:), allocatable :: new_key, this_key

   key = de%tok
   call eat_token(de, STRING, .true.)

   if (de%tok%tok == DOT) then
      ! create new key from token
      call key_from_token(this_key, key)
      call table%get_table(this_key, tptr)
      if (.not.associated(tptr)) then
         call table%push_back(this_key, tptr)
      end if
      deallocate(this_key)
      call next_token(de, .true.)
      call parse_keyval(de, tptr)
      return
   end if

   if (de%tok%tok /= EQUAL) then
      de%error = syntax_error(de%line, "missing =")
      return
   end if

   call next_token(de, .false.)

   ! create new key from token
   call key_from_token(new_key, key)
   if (.not.allocated(new_key)) then
      de%error = syntax_error(de%line, "invalid key")
      return
   end if

   select case(de%tok%tok)
   case(STRING) ! key = "value"
      call table%push_back(new_key, vptr)
      if (.not.associated(vptr)) then
         de%error = syntax_error(de%line, "key already exist in table")
         return
      end if
      vptr%val = de%tok%ptr(:de%tok%len)
      if (toml_get_value_type(vptr%val) == INVALID_TYPE) then
         de%error = syntax_error(de%line, "unknown value type")
         return
      end if
      call next_token(de, .true.)
   case(LBRACKET) ! key = [ array ]
      call table%push_back(new_key, aptr)
      if (.not.associated(aptr)) then
         de%error = key_exists_error(de%line, new_key)
         return
      end if
      call parse_array(de, aptr)
      if (allocated(de%error)) return
   case(LBRACE) ! key = { table }
      call table%push_back(new_key, tptr)
      if (.not.associated(tptr)) then
         de%error = key_exists_error(de%line, new_key)
         return
      end if
      call parse_table(de, tptr)
      if (allocated(de%error)) return
   case default
      de%error = syntax_error(de%line, "unexpected token")
      return
   end select
end subroutine parse_keyval

recursive subroutine parse_array(de, array)
   type(toml_deserializer_t), intent(inout) :: de
   type(toml_array_t), intent(inout) :: array
   integer(toml_kind_t) :: akind
   class(toml_value_t), pointer :: ptr
   logical :: first
   first = .true.
   call eat_token(de, LBRACKET, .false.)
   do
      call skip_newlines(de, .false.)
      if (de%tok%tok == RBRACKET) exit

      ! obtain kind of array
      akind = array%get_kind()

      select case(de%tok%tok)
      case(STRING) ! [ value, value ... ]
         if (akind == INVALID_KIND) then
            call array%new_keyval
            akind = array%get_kind()
         end if
         if (akind /= KEYVAL_KIND) then
            de%error = syntax_error(de%line, "a string array can only contain strings")
            return
         end if
         ! create new element in array
         call array%push_back(ptr)
         select type(ptr)
         type is(toml_keyval_t)
            ! copy raw value from token to value
            ptr%val = de%tok%ptr(:de%tok%len)
            if (first) then
               first = .false.
               array%type = toml_get_value_type(ptr%val)
            else
               if (toml_get_value_type(ptr%val) /= array%type) then
                  de%error = syntax_error(de%line, "array type mismatch while processing array values")
                  return
               end if
            end if
         class default
            de%error = vendor_error(de%line, "internal error")
            return
         end select
         call eat_token(de, STRING, .false.)
      case(LBRACKET) ! [ [array], [array] ...]
         if (akind == INVALID_KIND) then
            call array%new_array
            akind = array%get_kind()
         end if
         if (akind /= ARRAY_KIND) then
            de%error = syntax_error(de%line, "array type mismatch while processing array of arrays")
            return
         end if
         ! create new array in array
         call array%push_back(ptr)
         select type(ptr)
         type is(toml_array_t)
            call parse_array(de, ptr)
            if (allocated(de%error)) return
         class default
            de%error = vendor_error(de%line, "internal error")
            return
         end select
      case(LBRACE) ! [ {table}, {table} ... ]
         if (akind == INVALID_KIND) then
            call array%new_table
            akind = array%get_kind()
         end if
         if (akind /= TABLE_KIND) then
            de%error = syntax_error(de%line, "array type mismatch while processing array of tables")
            return
         end if
         ! create new table in array
         call array%push_back(ptr)
         select type(ptr)
         type is(toml_table_t)
            call parse_table(de, ptr)
            if (allocated(de%error)) return
         class default
            de%error = vendor_error(de%line, "internal error")
            return
         end select
      case default
         de%error = syntax_error(de%line, "unexpected token")
         return
      end select

      call skip_newlines(de, .false.)

      if (de%tok%tok == COMMA) then
         call eat_token(de, COMMA, .false.)
         cycle
      end if
      exit
   end do

   if (de%tok%tok /= RBRACKET) then
      de%error = syntax_error(de%line, "expects ]")
      return
   end if

   call eat_token(de, RBRACKET, .true.)
end subroutine parse_array

subroutine parse_table(de, table)
   type(toml_deserializer_t), intent(inout) :: de
   type(toml_table_t), intent(inout) :: table

   call eat_token(de, LBRACE, .true.)
   do
      if (de%tok%tok == NEWLINE) then
         de%error = syntax_error(de%line, "newline not allowed in inline table")
         return
      end if

      if (de%tok%tok == RBRACE) exit

      if (de%tok%tok /= STRING) then
         de%error = syntax_error(de%line, "expects string value")
         return
      end if

      call parse_keyval(de, table)
      if (allocated(de%error)) exit

      if (de%tok%tok == NEWLINE) then
         de%error = syntax_error(de%line, "newline not allowed in inline table")
         return
      end if

      if (de%tok%tok == COMMA) then
         call eat_token(de, COMMA, .true.)
         cycle
      end if
      exit
   end do

   if (de%tok%tok /= RBRACE) then
      de%error = syntax_error(de%line, "expects }")
      return
   end if

   call eat_token(de, RBRACE, .true.)
end subroutine parse_table

subroutine key_from_token(key, tok)
   character(len=:), allocatable, intent(out) :: key
   type(token_t), intent(in) :: tok
   call string_from_token(key, tok)
end subroutine key_from_token

subroutine string_from_token(str, tok)
   character(len=:), allocatable, intent(out) :: str
   type(token_t), intent(in) :: tok
   logical :: multiline, verbatim
   character(len=:), pointer :: start
   integer :: len
   start => tok%ptr
   len = tok%len
   verbatim = tok%ptr(1:1) == TOML_SQUOTE
   if (tok%ptr(1:1) == TOML_DQUOTE .or. verbatim) then
      multiline = verify(tok%ptr(1:3), TOML_DQUOTE) == 0 &
         &   .or. verify(tok%ptr(1:3), TOML_SQUOTE) == 0
      if (multiline) then
         start => start(4:)
         len = len - 6
      else
         start => start(2:)
         len = len - 2
      end if
      if (verbatim) then
         str = start(:len)
      else
         str = start(:len) ! FIXME
      end if

      if (scan(str, TOML_NEWLINE) > 0) then
         deallocate(str)
      end if

   else
      str = start(:len)
   end if
   nullify(start)
end subroutine string_from_token

subroutine eat_token(de, tok, dot_is_special)
   type(toml_deserializer_t), intent(inout) :: de
   integer(tokentype_t), intent(in) :: tok
   logical, intent(in) :: dot_is_special
   @:ASSERT(de%tok%tok == tok)
   call next_token(de, dot_is_special)
end subroutine

subroutine skip_newlines(de, dot_is_special)
   type(toml_deserializer_t), intent(inout) :: de
   logical, intent(in) :: dot_is_special
   do while(de%tok%tok == NEWLINE)
      call next_token(de, dot_is_special)
   end do
end subroutine skip_newlines

subroutine next_token(de, dot_is_special)
   type(toml_deserializer_t), intent(inout) :: de
   logical, intent(in) :: dot_is_special
   character(len=:), pointer :: ptr
   integer :: i
   ptr => de%tok%ptr

   !> consume token
   do i = 1, de%tok%len
      de%line%pos = de%line%pos + 1
      if (ptr(i:i) == TOML_NEWLINE) then
         de%line%ptr => ptr(i:i)
         de%line%num = de%line%num+1
         de%line%pos = 0
      end if
   end do
   ptr => ptr(de%tok%len+1:)

   !> make next token
   do while(len(ptr) > 0)
      select case(ptr(1:1))
      case('#')
         i = index(ptr, TOML_NEWLINE)
         if (i > 0) then
            ptr => ptr(i:)
            cycle
         end if
         exit
      case('.')
         if (dot_is_special) then
            de%tok = new_token(DOT, ptr, 1)
            return
         end if
      case(','); de%tok = new_token(COMMA, ptr, 1); return
      case('='); de%tok = new_token(EQUAL, ptr, 1); return
      case('{'); de%tok = new_token(LBRACE, ptr, 1); return
      case('}'); de%tok = new_token(RBRACE, ptr, 1); return
      case('[')
         if (ptr(1:1) == ptr(2:2)) then
            de%tok = new_token(LLBRACKET, ptr, 2)
         else
            de%tok = new_token(LBRACKET, ptr, 1)
         end if
         return
      case(']')
         if (ptr(1:1) == ptr(2:2)) then
            de%tok = new_token(RRBRACKET, ptr, 2)
         else
            de%tok = new_token(RBRACKET, ptr, 1)
         end if
         return
      case(TOML_NEWLINE); de%tok = new_token(NEWLINE, ptr, 1); return
      case(' ', char(9)); ptr => ptr(2:); cycle
      end select

      call scan_string(de, ptr, dot_is_special)
      return
   end do

   !> return with EOF token
   de%tok = new_token(EOF, ptr(1:0), 0)

contains

subroutine scan_string(de, ptr, dot_is_special)
   type(toml_deserializer_t), intent(inout) :: de
   character(len=:), pointer, intent(inout) :: ptr
   logical, intent(in) :: dot_is_special
   character(len=:), pointer :: orig
   integer :: i, skip
   integer :: hexreq
   integer :: qcount
   logical :: escape
   orig => ptr

   if (len(ptr) >= 6) then
      if (ptr(1:3) == repeat(TOML_SQUOTE, 3)) then
         ptr => ptr(4:)
         i = index(ptr, repeat(TOML_SQUOTE, 3))
         if (i == 0) then
            de%error = syntax_error(de%line, "unterminated triple-s-quote")
            return
         end if

         de%tok = new_token(STRING, orig, i+5)
         return
      end if

      if (ptr(1:3) == repeat(TOML_DQUOTE, 3)) then
         ptr => ptr(4:)
         escape = .false.
         hexreq = 0
         qcount = 0
         do i = 1, len(ptr)
            if (escape) then
               escape = .false.
               if (ptr(i:i) == 'u') then
                  hexreq = 4
                  cycle
               end if
               if (ptr(i:i) == 'U') then
                  hexreq = 8
                  cycle
               end if
               if (verify(ptr(i:i), 'btnfr"\') == 0) cycle
               ! allow for line ending backslash
               skip = verify(ptr(i:), TOML_WHITESPACE)-1
               if (ptr(i+skip:i+skip) == TOML_NEWLINE) cycle
               de%error = syntax_error(de%line, "bad escape char")
               return
            end if
            if (hexreq > 0) then
               hexreq = hexreq - 1
               if (verify(ptr(i:i), TOML_HEXDIGITS) == 0) cycle
               de%error = syntax_error(de%line, "expect hex char")
               return
            end if
            if (ptr(i:i) == '\') then
               escape = .true.
               cycle
            end if
            if (ptr(i:i) == TOML_DQUOTE) then
               qcount = qcount + 1
            else
               qcount = 0
            end if
            if (qcount == 3) then
               ptr => ptr(i:)
               exit
            end if
         end do
         if (qcount /= 3) then
            de%error = syntax_error(de%line, "unterminated triple-quote")
            return
         end if

         de%tok = new_token(STRING, orig, len(orig)-len(ptr)+1)
         return
      end if
   end if

   if (ptr(1:1) == TOML_SQUOTE) then
      ptr => ptr(2:)
      i = index(ptr, TOML_NEWLINE)
      if (i == 0) i = len(ptr)
      i = index(ptr(:i), TOML_SQUOTE)
      if (i == 0) then
         de%error = syntax_error(de%line, "unterminated s-quote")
         return
      end if

      de%tok = new_token(STRING, orig, i+1)
      return
   end if

   if (ptr(1:1) == TOML_DQUOTE) then
      ptr => ptr(2:)
      escape = .false.
      hexreq = 0
      do i = 1, len(ptr)
         if (escape) then
            escape = .false.
            if (ptr(i:i) == 'u') then
               hexreq = 4
               cycle
            end if
            if (ptr(i:i) == 'U') then
               hexreq = 8
               cycle
            end if
            if (verify(ptr(i:i), 'btnfr"\') == 0) cycle
            de%error = syntax_error(de%line, "bad escape char")
            return
         end if
         if (hexreq > 0) then
            hexreq = hexreq - 1
            if (verify(ptr(i:i), TOML_HEXDIGITS) == 0) cycle
            de%error = syntax_error(de%line, "expect hex char")
            return
         end if
         if (ptr(i:i) == '\') then
            escape = .true.
            cycle
         end if
         if (ptr(i:i) == TOML_NEWLINE) then
            ptr => ptr(i:)
            exit
         end if
         if (ptr(i:i) == TOML_DQUOTE) then
            ptr => ptr(i:)
            exit
         end if
      end do
      if (ptr(1:1) /= TOML_DQUOTE) then
            de%error = syntax_error(de%line, "expect hex char")
         return
      end if

      de%tok = new_token(STRING, orig, len(orig)-len(ptr)+1)
      return
   end if

   if (toml_raw_verify_date(ptr) .or. toml_raw_verify_time(ptr)) then
      i = verify(ptr, TOML_TIMESTAMP)-1
      if (i < 0) i = len(ptr)
      de%tok = new_token(STRING, orig, i)
      return
   end if

   do i = 1, len(ptr)
      if (ptr(i:i) == '.' .and. dot_is_special) then
         ptr => ptr(i:)
         exit
      end if
      if (verify(ptr(i:i), TOML_LITERALS) == 0) cycle
      ptr => ptr(i:)
      exit
   end do

   de%tok = new_token(STRING, orig, len(orig) - len(ptr))

end subroutine scan_string

end subroutine next_token

!> custom constructor to get pointer assignment right
type(token_t) function new_token(tok, ptr, len)
   integer(tokentype_t), intent(in) :: tok
   character(len=:), pointer, intent(in) :: ptr
   integer, intent(in) :: len
   new_token%tok = tok
   new_token%ptr => ptr
   new_token%len = len
end function new_token

!> Create a syntax error from a given line and a message.
type(error_t) elemental function syntax_error(line, message)
   type(line_t), intent(in) :: line
   character(len=*), intent(in) :: message
   syntax_error = error_t(line%num, message)
end function syntax_error

!> Create a syntax error because of an already existing key.
type(error_t) elemental function key_exists_error(line, key)
   type(line_t), intent(in) :: line
   character(len=*), intent(in) :: key
   key_exists_error = error_t(line%num, "key "//key//" already exists")
end function key_exists_error

!> We screwed up, explain why we couldn't solve this issue.
type(error_t) elemental function vendor_error(line, excuse)
   type(line_t), intent(in) :: line
   character(len=*), intent(in) :: excuse
   vendor_error = error_t(line%num, excuse)
end function vendor_error

!> Wrapper for error constructor to include full line and pointer to token.
type(error_t) function error_with_line(line, message)
   type(line_t), intent(in) :: line
   character(len=*), intent(in) :: message
   error_with_line = error_t(line%num, message // TOML_NEWLINE // &
      & '   | '// line%ptr(1:index(line%ptr, TOML_NEWLINE)-1) // TOML_NEWLINE // &
      & '   |' // repeat('-', line%pos) // '^')
end function error_with_line

end module tomlf08_de
