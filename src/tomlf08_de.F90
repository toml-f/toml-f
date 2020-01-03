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
   public :: toml_deserializer
   public :: toml_string_deserializer

   !> Possible token types in TOML.
   enum, bind(C)
      enumerator :: INVALID, &
         & DOT, COMMA, EQUAL, LBRACE, RBRACE, NEWLINE, LBRACKET, RBRACKET, &
         & LLBRACKET, RRBRACKET, STRING
   end enum
   !> Kind parameter for token type enums.
   integer, parameter :: tokentype = kind(INVALID)

   type :: de_line
      integer :: pos = 0
      integer :: num = 0
      character(len=:), pointer :: ptr => null()
   end type de_line

   type :: de_error
      integer :: lineno
      character(len=:), allocatable :: message
   end type de_error

   interface de_error
      module procedure :: error_with_line
   end interface

   !> TOML token.
   type :: token
      integer(tokentype) :: tok = INVALID
      character(len=:), pointer :: ptr => null()
      integer :: len = 0
   end type token

   !> TOML builder prototype.
   type, abstract :: toml_deserializer
      !> Signals if the tokenizer has finished (EOF has been reached).
      logical :: finished = .false.
      !> Current token.
      type(token) :: tok
      !> Root table.
      type(toml_table), allocatable :: root
      !> Pointer to the current table while transversing a table path.
      type(toml_table), pointer :: curtab => null()
      !> Current line (for error handling).
      type(de_line) :: line
      !> Error buffer, if allocated an error has occurred.
      type(de_error), allocatable :: error
   contains
      !> Produce the next token.
      procedure(tokenizer), deferred :: next_token
      !> Entry point for the parser.
      procedure :: parse => de_parse
      !> Parse a key-value pair.
      procedure, private :: parse_keyval => de_parse_keyval
      !> Parse an inline table.
      procedure, private :: parse_table => de_parse_table
      !> Parse an array.
      procedure, private :: parse_array => de_parse_array
      !> Parse a table or array header.
      procedure, private :: parse_select => de_parse_select
   end type

   !> TOML builder.
   type, extends(toml_deserializer) :: toml_string_deserializer
      !> Link to the input configuration.
      character(len=:), pointer :: conf
   contains
      procedure :: next_token => de_next_token
   end type

   interface toml_string_deserializer
      module procedure :: new_string_deserializer
   end interface toml_string_deserializer

   abstract interface
   subroutine tokenizer(de, dot_is_special, double_bracket)
      import toml_deserializer
      class(toml_deserializer), intent(inout) :: de
      logical, intent(in) :: dot_is_special, double_bracket
   end subroutine tokenizer
   end interface

contains

!> Constructor for the deserializer implementation.
type(toml_string_deserializer) function new_string_deserializer(conf) result(de)
   character(len=*), intent(in), target :: conf
   !> connect deserializer to configuration
   de%conf => conf
   de%line%ptr => conf
   de%line%num = 1
   !> first token is an artifical newline
   de%tok = new_token(NEWLINE, de%conf, 0)
end function new_string_deserializer

!> Entry point for the deserializer.
subroutine de_parse(de)
   class(toml_deserializer), intent(inout), target :: de

   !> create a new table
   allocate(de%root)
   de%curtab => de%root

   do while(.not.de%finished)
      select case(de%tok%tok)
      case(NEWLINE)
         call de%next_token(.true., .true.)
      case(STRING)
         call de%parse_keyval(de%curtab)
         if (allocated(de%error)) exit
         if (de%tok%tok /=  NEWLINE) then
            de%error = syntax_error(de%line, "extra chars after value")
            exit
         end if
         call de%next_token(.true., .true.)
      case(LBRACKET, LLBRACKET)
         call de%parse_select()
         if (allocated(de%error)) exit
      case default
         de%error = syntax_error(de%line, "syntax error")
         exit
      end select
   end do

end subroutine de_parse

!> Parse a table or array header.
subroutine de_parse_select(de)
   type :: tablenode
      character(len=:), allocatable :: key
      type(token) :: tok = token()
   end type
   type :: tablepath
      integer :: top = 0
      type(tablenode) :: node(10) = tablenode()
   end type tablepath
   class(toml_deserializer), intent(inout) :: de
   logical :: llb
   type(token) :: top
   character(len=:), allocatable :: key
   type(toml_table), pointer :: table
   type(toml_array), pointer :: array
   type(tablepath) :: path

   llb = de%tok%tok == LLBRACKET

   if (llb) then
      @:ASSERT(de%tok%tok == LLBRACKET)
      call de%next_token(.true., .true.)
   else
      @:ASSERT(de%tok%tok == LBRACKET)
      call de%next_token(.true., .true.)
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
      call de%next_token(.true., .true.)
   else
      if (de%tok%tok /= RBRACKET) then
         de%error = syntax_error(de%line, "expects ]")
         return
      end if
      call de%next_token(.true., .true.)
   end if

   if (de%tok%tok /= NEWLINE) then
      de%error = syntax_error(de%line, "extra chars after ] or ]]")
      return
   end if

contains

subroutine walk_tablepath(de, path)
   class(toml_deserializer), intent(inout), target :: de
   type(tablepath), intent(inout), target :: path
   type(toml_table), pointer :: table, ptr
   type(toml_array), pointer :: array
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
            type is(toml_table); ptr => elem
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
   class(toml_deserializer), intent(inout) :: de
   type(tablepath), intent(inout) :: path
   !> clear path
   path = tablepath()
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

      call de%next_token(.true., .true.)

      if (de%tok%tok == RBRACKET .or. de%tok%tok == RRBRACKET) exit

      if (de%tok%tok /= DOT) then
         de%error = syntax_error(de%line, "invalid key")
         return
      end if

      call de%next_token(.true., .true.)
   end do

   if (path%top <= 0) then
      de%error = syntax_error(de%line, "empty table selector")
   end if

end subroutine fill_tablepath

end subroutine de_parse_select

!> Parse a key-value pair.
recursive subroutine de_parse_keyval(de, table)
   class(toml_deserializer), intent(inout) :: de
   type(toml_table), intent(inout) :: table
   type(token) :: key
   type(toml_keyval), pointer :: vptr
   type(toml_array), pointer :: aptr
   type(toml_table), pointer :: tptr
   character(len=:), allocatable :: new_key, this_key

   key = de%tok
   @:ASSERT(de%tok%tok == STRING)
   call de%next_token(.true., .false.)

   if (de%tok%tok == DOT) then
      ! create new key from token
      call key_from_token(this_key, key)
      call table%get_table(this_key, tptr)
      if (.not.associated(tptr)) then
         call table%push_back(this_key, tptr)
      end if
      deallocate(this_key)
      call de%next_token(.true., .false.)
      if (de%tok%tok == STRING) then
         call de%parse_keyval(tptr)
      else
         de%error = syntax_error(de%line, "invalid key")
      end if
      return
   end if

   if (de%tok%tok /= EQUAL) then
      de%error = syntax_error(de%line, "missing =")
      return
   end if

   call de%next_token(.false., .false.)

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
      call de%next_token(.true., .false.)
   case(LBRACKET) ! key = [ array ]
      call table%push_back(new_key, aptr)
      if (.not.associated(aptr)) then
         de%error = key_exists_error(de%line, new_key)
         return
      end if
      call de%parse_array(aptr)
      if (allocated(de%error)) return
   case(LBRACE) ! key = { table }
      call table%push_back(new_key, tptr)
      if (.not.associated(tptr)) then
         de%error = key_exists_error(de%line, new_key)
         return
      end if
      call de%parse_table(tptr)
      if (allocated(de%error)) return
   case default
      de%error = syntax_error(de%line, "unexpected token")
      return
   end select
end subroutine de_parse_keyval

!> Parse an array.
recursive subroutine de_parse_array(de, array)
   class(toml_deserializer), intent(inout) :: de
   type(toml_array), intent(inout) :: array
   integer(toml_kind) :: akind
   class(toml_value), pointer :: ptr
   logical :: first
   first = .true.
   @:ASSERT(de%tok%tok == LBRACKET)
   call de%next_token(.false., .false.)
   do
      do while(de%tok%tok == NEWLINE)
         call de%next_token(.false., .false.)
      end do
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
         type is(toml_keyval)
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
         @:ASSERT(de%tok%tok == STRING)
         call de%next_token(.false., .false.)
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
         type is(toml_array)
            call de%parse_array(ptr)
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
         type is(toml_table)
            call de%parse_table(ptr)
            if (allocated(de%error)) return
         class default
            de%error = vendor_error(de%line, "internal error")
            return
         end select
      case default
         de%error = syntax_error(de%line, "unexpected token")
         return
      end select

      do while(de%tok%tok == NEWLINE)
         call de%next_token(.false., .false.)
      end do

      if (de%tok%tok == COMMA) then
         call de%next_token(.false., .false.)
         cycle
      end if
      exit
   end do

   if (de%tok%tok /= RBRACKET) then
      de%error = syntax_error(de%line, "expects ]")
      return
   end if

   call de%next_token(.true., .false.)
end subroutine de_parse_array

!> Parse an inline table.
subroutine de_parse_table(de, table)
   class(toml_deserializer), intent(inout) :: de
   type(toml_table), intent(inout) :: table

   @:ASSERT(de%tok%tok == LBRACE)
   call de%next_token(.true., .false.)
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

      call de%parse_keyval(table)
      if (allocated(de%error)) exit

      if (de%tok%tok == NEWLINE) then
         de%error = syntax_error(de%line, "newline not allowed in inline table")
         return
      end if

      if (de%tok%tok == COMMA) then
         call de%next_token(.true., .false.)
         cycle
      end if
      exit
   end do

   if (de%tok%tok /= RBRACE) then
      de%error = syntax_error(de%line, "expects }")
      return
   end if

   call de%next_token(.true., .false.)
end subroutine de_parse_table

subroutine key_from_token(key, tok)
   character(len=:), allocatable, intent(out) :: key
   type(token), intent(in) :: tok
   if (toml_raw_to_string(tok%ptr(:tok%len), key)) then
      if (index(key, TOML_NEWLINE) > 0) deallocate(key)
   else
      key = tok%ptr(:tok%len)
      if (verify(key, TOML_BAREKEY) > 0) deallocate(key)
   end if
end subroutine key_from_token

!> Produce the next token.
subroutine de_next_token(de, dot_is_special, double_bracket)
   class(toml_string_deserializer), intent(inout) :: de
   logical, intent(in) :: dot_is_special
   logical, intent(in) :: double_bracket
   character(len=:), pointer :: ptr
   integer :: i
   if (de%finished) return
   ptr => de%tok%ptr

   !> consume token
   do i = 1, de%tok%len
      de%line%pos = de%line%pos + 1
      if (ptr(i:i) == TOML_NEWLINE) then
         de%line%ptr => ptr(i:i)
         de%line%num = de%line%num+1
         de%line%pos = 1
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
         if (double_bracket .and. ptr(1:1) == ptr(2:2)) then
            de%tok = new_token(LLBRACKET, ptr, 2)
         else
            de%tok = new_token(LBRACKET, ptr, 1)
         end if
         return
      case(']')
         if (double_bracket .and. ptr(1:1) == ptr(2:2)) then
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
   de%finished = .true.
   de%tok = new_token(NEWLINE, ptr(1:0), 0)

contains

subroutine scan_string(de, ptr, dot_is_special)
   class(toml_string_deserializer), intent(inout) :: de
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

end subroutine de_next_token

!> custom constructor to get pointer assignment right
type(token) function new_token(tok, ptr, len)
   integer(tokentype), intent(in) :: tok
   character(len=:), pointer, intent(in) :: ptr
   integer, intent(in) :: len
   new_token%tok = tok
   new_token%ptr => ptr
   new_token%len = len
end function new_token

!> Create a syntax error from a given line and a message.
type(de_error) elemental function syntax_error(line, message)
   type(de_line), intent(in) :: line
   character(len=*), intent(in) :: message
   syntax_error = de_error(line%num, message)
end function syntax_error

!> Create a syntax error because of an already existing key.
type(de_error) elemental function key_exists_error(line, key)
   type(de_line), intent(in) :: line
   character(len=*), intent(in) :: key
   key_exists_error = de_error(line%num, "key "//key//" already exists")
end function key_exists_error

!> We screwed up, explain why we couldn't solve this issue.
type(de_error) elemental function vendor_error(line, excuse)
   type(de_line), intent(in) :: line
   character(len=*), intent(in) :: excuse
   vendor_error = de_error(line%num, excuse)
end function vendor_error

!> Wrapper for error constructor to include full line and pointer to token.
type(de_error) function error_with_line(line, message)
   type(de_line), intent(in) :: line
   character(len=*), intent(in) :: message
   error_with_line = de_error(line%num, message // TOML_NEWLINE // &
      & '   | '// line%ptr(1:index(line%ptr, TOML_NEWLINE)-1) // TOML_NEWLINE // &
      & '   |' // repeat('-', line%pos) // '^')
end function error_with_line

end module tomlf08_de
