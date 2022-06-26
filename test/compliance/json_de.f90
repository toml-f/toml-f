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

!> Implementation of a deserializer for a specific JSON format
module tftest_json_de
   use iso_fortran_env, only: error_unit
   use tomlf_build, only : get_value, set_value
   use tomlf_constants
   use tomlf_error, only : toml_stat, toml_error, toml_context, &
      & syntax_error, duplicate_key_error, vendor_error, io_error
   use tomlf_utils
   use tomlf_type, only : toml_value, toml_key, toml_table, toml_array, &
      & toml_keyval, toml_visitor, new_table, add_array, add_table, add_keyval, &
      & len
   implicit none
   private

   public :: json_parse


   !> Overloaded JSON parse interface
   interface json_parse
      module procedure :: json_parse_unit
      module procedure :: json_parse_string
   end interface json_parse


   !> Possible tokens in this specific version of JSON format
   type :: enum_tokentype

      !> Token does not match
      integer :: invalid = 0

      !> Separator for key and value
      integer :: colon = 1

      !> Separator for elements in an array
      integer :: comma = 2

      !> Whitespace and newlines are ignored
      integer :: whitespace = 3

      !> Left brace opening an object
      integer :: lbrace = 4

      !> Right brace closing an object
      integer :: rbrace = 5

      !> Left bracket opening an array
      integer :: lbracket = 6

      !> Right bracket closing an array
      integer :: rbracket = 7

      !> String value
      integer :: string = 8

   end type enum_tokentype

   !> Actual enumerator for possible tokentypes
   type(enum_tokentype), parameter :: json_tokentype = enum_tokentype()


   type, extends(toml_visitor) :: json_prune
   contains
      procedure :: visit
   end type json_prune


   !> Basic JSON token, produced by a JSON tokenizer
   type :: json_token

      !> Actual tokentype
      integer :: tok = json_tokentype%invalid

      !> Character representation of the token
      character(len=:), pointer :: ptr => null()

      !> Length of the token at ptr
      integer :: len = 0

   end type json_token


   !> Deserialization implementation for a specific JSON format
   type :: json_deserializer

      !> Signals if the tokenizer has finished (EOF has been reached)
      logical :: finished = .false.

      !> Current token
      type(json_token) :: tok

      !> Root table
      type(toml_table), allocatable :: root

      !> Pointer to the current table while transversing a table path
      type(toml_table), pointer :: current => null()

      !> Current line (for error handling)
      type(toml_context) :: line

      !> Error buffer, if allocated an error has occurred
      type(toml_error), allocatable :: error

      !> Link to the input configuration.
      character(len=:), pointer :: conf

   contains

      !> Entry point for parsing the JSON document, creates the root table
      procedure :: parse => parse_root
      procedure :: parse_table
      procedure :: parse_array
      procedure :: parse_keyval

      !> Get next token
      procedure :: next
      procedure :: next_token

   end type json_deserializer


contains


!> Constructor for the deserializer implementation.
subroutine new_deserializer(de, conf)

   !> Instance of the JSON deserializer
   type(json_deserializer), intent(out) :: de

   !> JSON representation of the TOML data structure
   character(len=*), intent(in), target :: conf

   ! connect deserializer to configuration
   de%conf => conf
   de%line%ptr => conf
   de%line%num = 1
   ! first token is an artifical whitespace
   de%tok = new_token(json_tokentype%whitespace, de%conf, 0)

end subroutine new_deserializer


!> Deserializer entry point for a formatted unit
subroutine json_parse_unit(table, unit, error)

   !> Instance of the deserialized TOML data structure
   type(toml_table), allocatable, intent(out) :: table

   !> Formatted unit to read from
   integer, intent(in) :: unit

   !> Error handling
   type(toml_error), allocatable, intent(out), optional :: error

   character(len=:), allocatable :: conf
   integer, parameter :: bufsize = 512
   character(len=bufsize) :: buffer
   character(len=bufsize) :: error_msg
   integer :: size
   integer :: stat

   !print*, 'enter json_parse_unit'

   allocate(character(len=0) :: conf)
   do 
      read(unit, '(a)', advance='no', iostat=stat, iomsg=error_msg, size=size) &
         & buffer
      if (stat > 0) exit
      conf = conf // buffer(:size)
      if (stat < 0) then
         if (is_iostat_eor(stat)) then
            stat = 0
            conf = conf // TOML_NEWLINE
         end if
         if (is_iostat_end(stat)) then
            stat = 0
            exit
         end if
      end if
   end do

   if (stat /= 0) then
      if (present(error)) then
         call io_error(error, trim(error_msg))
      else
         write(error_unit, '(a, /, a)') "IO runtime error", trim(error_msg)
      end if
      return
   end if

   call json_parse_string(table, conf, error)

end subroutine json_parse_unit


!> Deserializer entry point for a character variable
subroutine json_parse_string(table, conf, error)

   !> Instance of the deserialized TOML data structure
   type(toml_table), allocatable, intent(out) :: table

   !> String containing the JSON representation of the TOML data structure
   character(len=*), intent(in), target :: conf

   !> Error handling
   type(toml_error), allocatable, intent(out), optional :: error

   type(json_deserializer) :: de

   ! connect deserializer to configuration
   call new_deserializer(de, conf)

   call de%parse

   if (allocated(de%error)) then
      if (present(error)) then
         call move_alloc(de%error, error)
      else
         write(error_unit, '(a)') de%error%message
      end if
      return
   end if

   call move_alloc(de%root, table)

end subroutine json_parse_string


!> Entry point for parsing the TOML document, creates the root table
subroutine parse_root(de)

   !> Instance of the TOML deserializer
   class(json_deserializer), intent(inout), target :: de
   type(json_prune) :: pruner

   allocate(de%root)
   call new_table(de%root)

   do while(.not.de%finished)
      select case(de%tok%tok)
      case default
         call syntax_error(de%error, de%line, "syntax error")
         exit
      case(json_tokentype%whitespace)
         call de%next

      case(json_tokentype%lbrace)
         call de%parse_table(de%root)
         if (allocated(de%error)) exit

      end select
   end do
   if (allocated(de%error)) return

   call de%root%accept(pruner)

end subroutine parse_root


!> Parse a JSON object as TOML table
recursive subroutine parse_table(de, table)

   !> Instance of the TOML deserializer
   class(json_deserializer), intent(inout), target :: de

   !> TOML table to be filled
   type(toml_table), intent(inout) :: table

   !print*, 'enter parse_table'

   !@:ASSERT(de%tok%tok == LBRACE)
   call de%next
   do while(.not.de%finished)
      if (de%tok%tok == json_tokentype%rbrace) exit

      if (de%tok%tok /= json_tokentype%string) then
         call syntax_error(de%error, de%line, "expects string value")
         return
      end if

      call de%parse_keyval(table)
      if (allocated(de%error)) exit

      if (de%tok%tok == json_tokentype%string) then
         call syntax_error(de%error, de%line, "newline not allowed in inline table")
         return
      end if

      if (de%tok%tok == json_tokentype%comma) then
         call de%next
         cycle
      end if
      exit
   end do

   if (de%tok%tok /= json_tokentype%rbrace) then
      call syntax_error(de%error, de%line, "expects }")
      return
   end if

   call de%next

   !print*, 'exit parse_table', de%finished
end subroutine parse_table


!> Parse a JSON value as TOML key-value pair
subroutine parse_keyval(de, table)

   !> Instance of the TOML deserializer
   class(json_deserializer), intent(inout), target :: de

   !> Current TOML table
   type(toml_table), intent(inout) :: table

   type(json_token) :: key
   type(toml_keyval), pointer :: vptr
   type(toml_array), pointer :: aptr
   type(toml_table), pointer :: tptr
   character(kind=tfc, len=:), allocatable :: new_key

   !print*, 'enter parse_keyval'

   key = de%tok
   !@:ASSERT(de%tok%tok == STRING)
   call de%next

   if (de%tok%tok /= json_tokentype%colon) then
      call syntax_error(de%error, de%line, "missing =")
      return
   end if

   call de%next
   if (allocated(de%error)) return

   ! create new key from token
   call key_from_token(new_key, key)
   if (.not.allocated(new_key)) then
      call syntax_error(de%error, de%line, "invalid key")
      return
   end if

   select case(de%tok%tok)
   case default
      call syntax_error(de%error, de%line, "unexpected token")
      return

   case(json_tokentype%string) ! key = "value"
      call add_keyval(table, new_key, vptr)
      if (.not.associated(vptr)) then
         call duplicate_key_error(de%error, de%line, new_key)
         return
      end if
      vptr%raw = de%tok%ptr(:de%tok%len)
      if (toml_get_value_type(vptr%raw) == toml_type%invalid) then
         call syntax_error(de%error, de%line, "unknown value type")
         return
      end if
      call de%next
      if (allocated(de%error)) return

   case(json_tokentype%lbracket) ! key = [ array ]
      call add_array(table, new_key, aptr)
      if (.not.associated(aptr)) then
         call duplicate_key_error(de%error, de%line, new_key)
         return
      end if
      call de%parse_array(aptr)
      if (allocated(de%error)) return

   case(json_tokentype%lbrace) ! key = { table }
      call add_table(table, new_key, tptr)
      if (.not.associated(tptr)) then
         call duplicate_key_error(de%error, de%line, new_key)
         return
      end if
      call de%parse_table(tptr)
      if (allocated(de%error)) return

   end select

   !print*, 'exit parse_keyval', de%finished
end subroutine parse_keyval


!> Parse a JSON array as TOML array
recursive subroutine parse_array(de, array)

   !> Instance of the TOML deserializer
   class(json_deserializer), intent(inout), target :: de

   !> Current TOML table
   type(toml_array), intent(inout) :: array

   type(toml_table), pointer :: tbl
   type(toml_keyval), pointer :: val
   type(toml_array), pointer :: arr

   !@:assert(de%tok%tok == json_tokentype%lbracket)
   !print*, 'enter parse_array'

   call de%next
   do while(.not.de%finished)
      if (de%tok%tok == json_tokentype%rbracket) then
         exit
      end if

      select case(de%tok%tok)
      case default
         call syntax_error(de%error, de%line, "unexpected token")
         return

      case(json_tokentype%string) ! [ value, value ... ]
         call add_keyval(array, val)
         val%raw = de%tok%ptr(:de%tok%len)

         call de%next

      case(json_tokentype%lbracket) ! [ [array], [array] ...]
         call add_array(array, arr)
         arr%inline = .true.
         call de%parse_array(arr)
         if (allocated(de%error)) return

      case(json_tokentype%lbrace) ! [ {table}, {table} ... ]
         call add_table(array, tbl)
         tbl%inline = .true.
         call de%parse_table(tbl)
         if (allocated(de%error)) return

      end select

      if (de%tok%tok == json_tokentype%comma) then
         call de%next
         cycle
      end if
      exit
   end do

   if (de%tok%tok /= json_tokentype%rbracket) then
      call syntax_error(de%error, de%line, "expects ]")
      return
   end if

   call de%next
   !print*, 'exit parse_array', de%finished

end subroutine parse_array


!> Return next token
subroutine next_token(de)

   !> Instance of the tokenizer
   class(json_deserializer), intent(inout) :: de

   character(len=:), pointer :: ptr, orig
   integer :: i, skip
   integer :: hexreq
   logical :: escape

   if (de%finished) return
   ptr => de%tok%ptr

   ! consume token
   do i = 1, de%tok%len
      de%line%pos = de%line%pos + 1
      if (ptr(i:i) == TOML_NEWLINE) then
         de%line%ptr => ptr(min(i+1, len(ptr)):)
         de%line%num = de%line%num+1
         de%line%pos = 1
      end if
   end do
   !print*, de%tok%tok, ptr(:de%tok%len)
   ptr => ptr(de%tok%len+1:)

   do while(len(ptr) > 0)
      select case(ptr(1:1))
      case default; de%tok = new_token(json_tokentype%invalid, ptr, 1); return
      case(','); de%tok = new_token(json_tokentype%comma, ptr, 1); return
      case(':'); de%tok = new_token(json_tokentype%colon, ptr, 1); return
      case('{'); de%tok = new_token(json_tokentype%lbrace, ptr, 1); return
      case('}'); de%tok = new_token(json_tokentype%rbrace, ptr, 1); return
      case('['); de%tok = new_token(json_tokentype%lbracket, ptr, 1); return
      case(']'); de%tok = new_token(json_tokentype%rbracket, ptr, 1); return
      case(' ', char(9), TOML_NEWLINE);
         skip = verify(ptr, TOML_WHITESPACE//TOML_NEWLINE)-1
         if (skip < 0) exit
         de%tok = new_token(json_tokentype%whitespace, ptr, skip)
         return
      case('"')
         orig => ptr
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
               call syntax_error(de%error, de%line, "bad escape char")
               return
            end if
            if (hexreq > 0) then
               hexreq = hexreq - 1
               if (verify(ptr(i:i), TOML_HEXDIGITS) == 0) cycle
               call syntax_error(de%error, de%line, "expect hex char")
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
            call syntax_error(de%error, de%line, "expect hex char")
            return
         end if

         de%tok = new_token(json_tokentype%string, orig, len(orig)-len(ptr)+1)
         return
      end select

   end do

   ! return with EOF token
   de%finished = .true.
   de%tok = new_token(json_tokentype%whitespace, ptr, 0)

end subroutine next_token


!> Return next token
subroutine next(de)

   !> Instance of the tokenizer
   class(json_deserializer), intent(inout) :: de

   call de%next_token
   do while(de%tok%tok == json_tokentype%whitespace .and. .not.de%finished)
      if (allocated(de%error)) exit
      call de%next_token
   end do

end subroutine next


!> custom constructor to get pointer assignment right
function new_token(tok, ptr, len)

   !> Token type
   integer, intent(in) :: tok

   !> Current position in the character variable
   character(len=:), pointer, intent(in) :: ptr

   !> Length of the token
   integer, intent(in) :: len

   !> Newly produced token
   type(json_token) :: new_token

   new_token%tok = tok
   new_token%ptr => ptr
   new_token%len = len

end function new_token


!> Visit a TOML value
subroutine visit(self, val)

   !> Instance of the JSON pruner
   class(json_prune), intent(inout) :: self

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   select type(val)
   class is(toml_array)
      call visit_array(self, val)
   class is(toml_table)
      call visit_table(self, val)
   end select

end subroutine visit


!> Visit a TOML array
subroutine visit_array(visitor, array)

   !> Instance of the JSON pruner
   class(json_prune), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_array), intent(inout) :: array

   class(toml_value), allocatable :: val, tmp
   character(kind=tfc, len=:), allocatable :: str
   type(toml_key), allocatable :: vt(:)
   integer :: i, n, stat

   n = len(array)
   do i = 1, n
      call array%shift(val)
      select type(val)
      class default
         call val%accept(visitor)
      class is(toml_table)
         call val%get_keys(vt)
         if (val%has_key("type") .and. val%has_key("value") .and. size(vt)==2) then
            call get_value(val, "type", str)
            call prune(tmp, val, str)
            call val%destroy
            call tmp%accept(visitor)
            call array%push_back(tmp, stat)
            cycle
         else
            call val%accept(visitor)
         end if
      end select
      call array%push_back(val, stat)
   end do

end subroutine visit_array


!> Visit a TOML table
subroutine visit_table(visitor, table)

   !> Instance of the JSON pruner
   class(json_prune), intent(inout) :: visitor

   !> TOML table to visit
   type(toml_table), intent(inout) :: table

   class(toml_value), pointer :: ptr
   class(toml_value), allocatable :: val
   character(kind=tfc, len=:), allocatable :: str
   type(toml_key), allocatable :: list(:), vt(:)
   integer :: i, n, stat

   call table%get_keys(list)
   n = size(list, 1)

   do i = 1, n
      call table%get(list(i)%key, ptr)
      select type(ptr)
      class default
         call ptr%accept(visitor)
      class is(toml_table)
         call ptr%get_keys(vt)
         if (ptr%has_key("type") .and. ptr%has_key("value") .and. size(vt)==2) then
            call get_value(ptr, "type", str)
            call prune(val, ptr, str)
            call val%accept(visitor)
            call table%delete(list(i)%key)
            call table%push_back(val, stat)
         else
            call ptr%accept(visitor)
         end if
      end select
   end do

end subroutine visit_table


subroutine prune(val, table, str)

   !> Actual TOML value
   class(toml_value), allocatable, intent(out) :: val

   !> TOML table to prune
   type(toml_table), intent(inout) :: table

   !> Value kind
   character(kind=tfc, len=*), intent(in) :: str

   class(toml_value), pointer :: ptr
   character(kind=tfc, len=:), allocatable :: tmp
   integer :: stat
   real(tfr) :: fval

   call table%get("value", ptr)
   allocate(val, source=ptr)
   if (allocated(table%key)) then
      val%key = table%key
   else
      deallocate(val%key)
   end if

   select type(val)
   class is(toml_keyval)
      select case(str)
      case("integer", "bool", "data", "time", "datetime")
         call move_alloc(val%raw, tmp)
         call unquote_json(val%raw, tmp)
      case("float")
         call move_alloc(val%raw, tmp)
         call unquote_json(val%raw, tmp)
         read(val%raw, *, iostat=stat) fval
         if (stat == 0) then
            deallocate(val%raw)
            call set_value(val, fval)
         end if
      end select

   end select

end subroutine prune


!> Generate a key
subroutine key_from_token(key, tok)

   !> TOML raw key
   character(kind=tfc, len=:), allocatable, intent(out) :: key

   !> String token containing the possible key
   type(json_token), intent(in) :: tok

   if (toml_raw_to_string(tok%ptr(:tok%len), key)) then
      if (index(key, toml_escape%newline) > 0) deallocate(key)
   else
      key = tok%ptr(:tok%len)
      if (verify(key, TOML_BAREKEY) > 0) deallocate(key)
   end if

end subroutine key_from_token


!> Transform a TOML raw value to a JSON compatible escaped string
subroutine unquote_json(raw, string)

   !> JSON compatible escaped string
   character(len=*), intent(in) :: string

   !> Raw value of TOML value
   character(len=:), allocatable, intent(out) :: raw

   integer :: i
   logical :: escape

   raw = ''
   escape = .false.
   do i = 2, len(string)-1
      if (escape) then
         select case(string(i:i))
         case default; raw = raw // string(i:i)
         case('\'); raw = raw // '\'
         case('"'); raw = raw // '"'
         case('n'); raw = raw // TOML_NEWLINE
         case('f'); raw = raw // TOML_FORMFEED
         case('r'); raw = raw // TOML_CARRIAGE_RETURN
         case('t'); raw = raw // TOML_TABULATOR
         case('b'); raw = raw // TOML_BACKSPACE
         end select
         escape = .false.
      else
         select case(string(i:i))
         case default; raw = raw // string(i:i)
         case('\'); escape = .true.
         end select
      end if
   end do

end subroutine unquote_json


end module tftest_json_de
