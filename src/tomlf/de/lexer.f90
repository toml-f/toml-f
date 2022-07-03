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

module tomlf_de_lexer
   use tomlf_constants, only : tfc, tfi, tfr, TOML_BACKSPACE, TOML_TABULATOR, TOML_NEWLINE, &
      & TOML_CARRIAGE_RETURN, TOML_FORMFEED
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time
   use tomlf_de_context, only : toml_context
   use tomlf_de_token, only : toml_token, stringify, token_kind, resize
   use tomlf_error, only : toml_error, toml_stat
   use tomlf_type, only : toml_table
   implicit none
   private

   public :: toml_lexer, new_lexer_from_file, new_lexer_from_unit, new_lexer_from_string
   public :: toml_token, stringify, token_kind


   type :: enum_char
      character(1, tfc) :: space = tfc_" "
      character(1, tfc) :: hash = tfc_"#"
      character(1, tfc) :: squote = tfc_"'"
      character(1, tfc) :: squote3(3) = [tfc_"'", tfc_"'", tfc_"'"]
      character(1, tfc) :: dquote = tfc_""""
      character(1, tfc) :: dquote3(3) = [tfc_"""", tfc_"""", tfc_""""]
      character(1, tfc) :: backslash = tfc_"\"
      character(1, tfc) :: dot = tfc_"."
      character(1, tfc) :: comma = tfc_","
      character(1, tfc) :: equal = tfc_"="
      character(1, tfc) :: lbrace = tfc_"{"
      character(1, tfc) :: rbrace = tfc_"}"
      character(1, tfc) :: lbracket = tfc_"["
      character(1, tfc) :: rbracket = tfc_"]"
      character(1, tfc) :: newline = achar(10, kind=tfc)
      character(1, tfc) :: formfeed = achar(12, kind=tfc)
      character(1, tfc) :: carriage_return = achar(13, kind=tfc)
      character(1, tfc) :: bspace = achar(8, kind=tfc)
      character(1, tfc) :: tab = achar(9, kind=tfc)
      character(1, tfc) :: plus = tfc_"+"
      character(1, tfc) :: minus = tfc_"-"
      character(12, tfc) :: literal = tfc_"0123456789-_"
   end type enum_char

   type(enum_char), parameter :: char_kind = enum_char()

   character(*, tfc), parameter :: terminated = &
      & char_kind%space//char_kind%tab//char_kind%newline//char_kind%carriage_return//&
      & char_kind%hash//char_kind%rbrace//char_kind%rbracket//char_kind%comma//&
      & char_kind%equal

   type :: enum_scope
      integer :: table = 1
      integer :: equal = 2
      integer :: array = 3
   end type enum_scope

   type(enum_scope), parameter :: lexer_scope = enum_scope()

   type :: stack_item
      integer :: scope
      integer :: token
   end type stack_item


   type :: toml_lexer
      character(len=:), allocatable :: source
      integer :: pos = 0
      character(1, tfc), allocatable :: chunk(:)
      integer :: top = 0
      type(stack_item), allocatable :: stack(:)
      integer :: buffer = 0
      type(toml_context) :: context
   contains
      procedure :: next
      generic :: extract => extract_string, extract_integer, extract_float, &
         & extract_bool, extract_datetime
      procedure :: extract_string
      procedure :: extract_integer
      procedure :: extract_float
      procedure :: extract_bool
      procedure :: extract_datetime
      procedure :: extract_raw
   end type toml_lexer

   interface resize
      module procedure :: resize_scope
   end interface

contains

subroutine new_lexer_from_file(lexer, filename, error)
   type(toml_lexer), intent(out) :: lexer
   character(len=*), intent(in) :: filename
   type(toml_error), allocatable, intent(out) :: error

   integer :: stat, io, length

   lexer%pos = 0
   lexer%source = filename
   open(file=filename, &
      & status="old", &
      & access="stream", & 
      & position="append", &
      & newunit=io, &
      & iostat=stat)
   if (stat == 0) then
      inquire(unit=io, pos=length)
      allocate(lexer%chunk(length-1), stat=stat)
   end if
   if (stat == 0) then
      read(io, pos=1, iostat=stat) lexer%chunk(:length-1)
   end if
   if (stat == 0) then
      close(io)
   end if
   call resize(lexer%stack)

   if (stat /= 0) then
      allocate(error)
      error%message = "failed to read from '"//filename//"'"
      error%stat = toml_stat%fatal
   end if
end subroutine new_lexer_from_file


subroutine new_lexer_from_unit(lexer, io, error)
   type(toml_lexer), intent(out) :: lexer
   integer, intent(in) :: io
   type(toml_error), allocatable, intent(out) :: error

   character(:, tfc), allocatable :: source
   integer, parameter :: bufsize = 512
   character(len=bufsize) :: buffer, msg
   integer :: length, stat, pos

   inquire(unit=io, access=msg, name=buffer)
   lexer%pos = 0
   if (len_trim(buffer) > 0) lexer%source = trim(buffer)
   select case(trim(msg))
   case("stream", "STREAM")
      stat = 1

   case("sequential", "SEQUENTIAL")
      allocate(character(len=0) :: source)
      do 
         read(io, '(a)', advance='no', iostat=stat, iomsg=msg, size=length) &
            & buffer
         if (stat > 0) exit
         source = source // buffer(:length)
         if (stat < 0) then
            if (is_iostat_eor(stat)) then
               stat = 0
               source = source // TOML_NEWLINE
            end if
            if (is_iostat_end(stat)) then
               stat = 0
               exit
            end if
         end if
      end do
      call new_lexer_from_string(lexer, source)

   case("direct", "DIRECT")
      stat = 1

   end select

   if (stat /= 0) then
      allocate(error)
      if (len_trim(msg) > 0) then
         error%message = trim(msg)
      else
         error%message = "failed to read from unit"
      end if
      error%stat = toml_stat%fatal
   end if
end subroutine new_lexer_from_unit


subroutine new_lexer_from_string(lexer, string)
   type(toml_lexer), intent(out) :: lexer
   character(len=*), intent(in) :: string

   integer :: length
   character(1, tfc) :: ch

   length = len(string)
   lexer%pos = 0
   lexer%buffer = 0
   allocate(lexer%chunk(length))
   lexer%chunk(:length) = transfer(string, ch, length)
   call resize(lexer%stack)
end subroutine new_lexer_from_string


subroutine next(lexer, token)
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   if (lexer%buffer >= lexer%context%top) then
      call fill_buffer(lexer)
   end if

   lexer%buffer = lexer%buffer + 1
   token = lexer%context%token(lexer%buffer)
end subroutine next


subroutine fill_buffer(lexer)
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer

   type(toml_token) :: token
   integer :: stack_top, it

   lexer%buffer = 0
   lexer%context%top = 0
   stack_top = lexer%top

   ! Tokenization will cover always a complete scope
   do while(lexer%top >= stack_top .and. token%kind /= token_kind%eof)
      call next_token(lexer, token)
      call lexer%context%push_back(token)
   end do

   ! Flag all incomplete inline table and array scopes for the parser
   if (lexer%top > stack_top) then
      do it = lexer%top, stack_top + 1, -1
         select case(lexer%stack(it)%scope)
         case(lexer_scope%table, lexer_scope%array)
            lexer%context%token(lexer%stack(it)%token)%kind = token_kind%unclosed
         end select
      end do
   end if
end subroutine fill_buffer


subroutine next_token(lexer, token)
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   integer :: prev, pos
   character(1, tfc) :: ch

   ! Consume current token
   lexer%pos = lexer%pos + token%last - token%first + 1
   prev = lexer%pos
   pos = lexer%pos

   ! If lexer is exhausted, return EOF as early as possible
   if (pos > size(lexer%chunk)) then
      call pop(lexer, lexer_scope%equal)
      token = toml_token(token_kind%eof, prev, pos)
      return
   end if

   select case(peek(lexer, pos))
   case(char_kind%hash)
      do while(all(peek(lexer, pos+1) /= [char_kind%carriage_return, char_kind%newline]) &
            & .and. pos <= size(lexer%chunk))
         pos = pos + 1
      end do
      token = toml_token(token_kind%comment, prev, pos)

   case(char_kind%space, char_kind%tab)
      do while(any(match(lexer, pos+1, [char_kind%space, char_kind%tab])) &
            & .and. pos <= size(lexer%chunk))
         pos = pos + 1
      end do
      token = toml_token(token_kind%whitespace, prev, pos)

   case(char_kind%newline)
      call pop(lexer, lexer_scope%equal)
      token = toml_token(token_kind%newline, prev, pos)

   case(char_kind%carriage_return)
      if (match(lexer, pos+1, char_kind%newline)) then
         pos = pos + 1
         call pop(lexer, lexer_scope%equal)
         token = toml_token(token_kind%newline, prev, pos)
      else
         token = toml_token(token_kind%invalid, prev, pos)
      end if

   case(char_kind%dot)
      if (view_scope(lexer) == lexer_scope%table) then
         token = toml_token(token_kind%dot, prev, pos)
      else
         token = toml_token(token_kind%invalid, prev, pos)
      end if

   case(char_kind%comma)
      call pop(lexer, lexer_scope%equal)
      token = toml_token(token_kind%comma, prev, pos)

   case(char_kind%equal)
      token = toml_token(token_kind%equal, prev, pos)
      call push_back(lexer, lexer_scope%equal, lexer%context%top + 1)

   case(char_kind%lbrace)
      token = toml_token(token_kind%lbrace, prev, pos)
      call push_back(lexer, lexer_scope%table, lexer%context%top + 1)

   case(char_kind%rbrace)
      call pop(lexer, lexer_scope%equal)
      call pop(lexer, lexer_scope%table)
      token = toml_token(token_kind%rbrace, prev, pos)

   case(char_kind%lbracket)
      token = toml_token(token_kind%lbracket, prev, pos)
      if (any(view_scope(lexer) == [lexer_scope%equal, lexer_scope%array])) then
         call push_back(lexer, lexer_scope%array, lexer%context%top + 1)
      end if

   case(char_kind%rbracket)
      call pop(lexer, lexer_scope%array)
      token = toml_token(token_kind%rbracket, prev, pos)

   case(char_kind%squote)
      call next_sstring(lexer, token)

   case(char_kind%dquote)
      call next_dstring(lexer, token)

   case default
      if (view_scope(lexer) == lexer_scope%table) then
         call next_keypath(lexer, token)
      else
         call next_literal(lexer, token)
      end if

   end select
end subroutine next_token

subroutine next_sstring(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   character(1, tfc) :: ch
   integer :: prev, pos, it
   logical :: valid

   prev = lexer%pos
   pos = lexer%pos

   if (all(match(lexer, [pos+1, pos+2], char_kind%squote))) then
      pos = pos + 3

      pos = strstr(lexer%chunk(pos:), char_kind%squote3) + pos - 1
      if (pos < prev + 3) then
         token = toml_token(token_kind%invalid, prev, size(lexer%chunk))
         return
      end if

      do it = 1, 2
         if (match(lexer, pos+3, char_kind%squote)) pos = pos + 1
      end do

      valid = .true.
      do it = prev + 3, pos - 1
         ch = peek(lexer, it)
         valid = valid .and. valid_string(ch)
      end do

      token = toml_token(merge(token_kind%mliteral, token_kind%invalid, valid), prev, pos+2)
      return
   end if

   valid = .true.

   do while(pos < size(lexer%chunk))
      pos = pos + 1
      ch = peek(lexer, pos)
      valid = valid .and. valid_string(ch)
      if (ch == char_kind%squote) exit
      if (ch == char_kind%newline) then
         pos = pos - 1
         valid = .false.
         exit
      end if
   end do

   valid = valid .and. peek(lexer, pos) == char_kind%squote .and. pos /= prev
   token = toml_token(merge(token_kind%literal, token_kind%invalid, valid), prev, pos)
end subroutine next_sstring

subroutine next_dstring(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   character(1, tfc) :: ch
   character(*, tfc), parameter :: hexnum = "0123456789ABCDEF", valid_escape = "btnfr\"""
   integer :: prev, pos, expect, it
   logical :: escape, valid, space

   prev = lexer%pos
   pos = lexer%pos

   if (all(match(lexer, [pos+1, pos+2], char_kind%dquote))) then
      pos = pos + 3

      do
         pos = strstr(lexer%chunk(pos:), char_kind%dquote3) + pos - 1
         if (pos < prev + 3) then
            token = toml_token(token_kind%invalid, prev, size(lexer%chunk))
            return
         end if

         if (match(lexer, pos-1, char_kind%backslash)) then
            pos = pos + 1
            cycle
         end if

         do it = 1, 2
            if (match(lexer, pos+3, char_kind%dquote)) pos = pos + 1
         end do
         exit
      end do

      valid = .true.
      escape = .false.
      space = .false.
      expect = 0

      do it = prev + 3, pos - 1
         ch = peek(lexer, it)
         if (escape) then
            space = verify(ch, char_kind%space//char_kind%tab//&
               & char_kind%carriage_return//char_kind%newline) == 0
         end if
         if (space) then
            escape = .false.
            if (ch == char_kind%newline) then
               if (expect > 0) expect = expect - 1
               space = .false.
               cycle
            end if
            if (verify(ch, char_kind%space//char_kind%tab) == 0 .and. expect == 0) cycle
            if (ch == char_kind%carriage_return) then
               expect = 1
               cycle
            end if
            valid = .false.
            space = .false.
            expect = 0
            cycle
         end if
         valid = valid .and. valid_string(ch)
         if (escape) then
            escape = .false.
            space = .false.
            if (verify(ch, valid_escape) == 0) cycle
            if (ch == "u") then
               expect = 4
               cycle
            end if
            if (ch == "U") then
               expect = 8
               cycle
            end if
            valid = .false.
            cycle
         end if
         if (expect > 0) then
            expect = expect - 1
            valid = valid .and. verify(ch, hexnum) == 0
            cycle
         end if
         escape = ch == char_kind%backslash
      end do

      ! Check for any unfinished escape sequences
      valid = valid .and. expect == 0 .and. .not.(escape.or.space)

      token = toml_token(merge(token_kind%mstring, token_kind%invalid, valid), prev, pos+2)
      return
   end if

   valid = .true.
   escape = .false.
   expect = 0

   do while(pos < size(lexer%chunk))
      pos = pos + 1
      ch = peek(lexer, pos)
      valid = valid .and. valid_string(ch)
      if (escape) then
         escape = .false.
         if (verify(ch, valid_escape) == 0) cycle
         if (ch == "u") then
            expect = 4
            cycle
         end if
         if (ch == "U") then
            expect = 8
            cycle
         end if
         valid = .false.
         cycle
      end if
      if (expect > 0) then
         expect = expect - 1
         valid = valid .and. verify(ch, hexnum) == 0
         cycle
      end if
      escape = ch == char_kind%backslash
      if (ch == char_kind%dquote) exit
      if (ch == char_kind%newline) then
         pos = pos - 1
         valid = .false.
         exit
      end if
   end do

   valid = valid .and. peek(lexer, pos) == char_kind%dquote .and. pos /= prev
   token = toml_token(merge(token_kind%string, token_kind%invalid, valid), prev, pos)
end subroutine next_dstring

pure function valid_string(ch) result(valid)
   character(1, tfc), intent(in) :: ch
   logical :: valid

   character(1, tfc), parameter :: x00 = achar(int(z"00")), x08 = achar(int(z"08")), &
      & x0b = achar(int(z"0b")), x1f = achar(int(z"1f")), x7f = achar(int(z"7f"))

   valid = &
      & .not.(x00 <= ch .and. ch <= x08) .and. &
      & .not.(x0b <= ch .and. ch <= x1f) .and. &
      & ch /= x7f
end function

subroutine next_keypath(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   logical :: valid
   integer :: prev, pos
   character(1, tfc) :: ch

   prev = lexer%pos
   pos = lexer%pos
   ch = peek(lexer, pos)

   valid = (tfc_"A" <= ch .and. ch <= tfc_"Z") &
      & .or. (tfc_"a" <= ch .and. ch <= tfc_"z") &
      & .or. (verify(ch, char_kind%literal) == 0)
   do while(verify(peek(lexer, pos+1), terminated//char_kind%dot) > 0)
      pos = pos + 1
      ch = peek(lexer, pos)

      if (tfc_"A" <= ch .and. ch <= tfc_"Z") cycle
      if (tfc_"a" <= ch .and. ch <= tfc_"z") cycle
      if (verify(ch, char_kind%literal) == 0) cycle

      valid = .false.
      cycle
   end do

   token = toml_token(merge(token_kind%keypath, token_kind%invalid, valid), prev, pos)
end subroutine next_keypath

!> Identify literal values
subroutine next_literal(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   integer :: prev, pos
   integer, parameter :: offset(*) = [0, 1, 2, 3, 4, 5]
   character(1, tfc), parameter :: nan(3) = ["n", "a", "n"], inf(3) = ["i", "n", "f"], &
      & true(4) = ["t", "r", "u", "e"], false(5) = ["f", "a", "l", "s", "e"]

   prev = lexer%pos
   pos = lexer%pos

   select case(peek(lexer, pos))
   case("t")
      if (match_all(lexer, pos+offset(:4), true) .and. &
         & verify(peek(lexer, pos+4), terminated) == 0) then
         token = toml_token(token_kind%bool, prev, pos+3)
         return
      end if

   case("f")
      if (match_all(lexer, pos+offset(:5), false) .and. &
         & verify(peek(lexer, pos+5), terminated) == 0) then
         token = toml_token(token_kind%bool, prev, pos+4)
         return
      end if

   case default
      call next_datetime(lexer, token)
      if (token%kind == token_kind%datetime) return

      call next_integer(lexer, token)
      if (token%kind == token_kind%int) return

      call next_float(lexer, token)
      if (token%kind == token_kind%float) return

   end select

   ! If the current token is invalid, advance to the next terminator
   do while(verify(peek(lexer, pos+1), terminated) > 0)
      pos = pos + 1
   end do
   token = toml_token(token_kind%invalid, prev, pos)
end subroutine next_literal


subroutine next_integer(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   character(*, tfc), parameter :: toml_base(4) = [&
      & "0123456789abcdefABCDEF", &
      & "0123456789000000000000", &
      & "0123456700000000000000", &
      & "0100000000000000000000"]
   integer, parameter :: b10 = 2, b16 = 1, b8 = 3, b2 = 4

   character(1, tfc) :: ch
   integer :: prev, pos, base
   logical :: underscore, okay

   prev = lexer%pos
   pos = lexer%pos
   okay = .true.
   underscore = .true.
   base = b10

   if (any(match(lexer, pos, ["+", "-"]))) then
      pos = pos + 1
   end if

   if (match(lexer, pos, "0")) then
      select case(peek(lexer, pos+1))
      case("x")
         okay = pos == prev
         base = b16
         pos = pos + 2
      case("o")
         okay = pos == prev
         base = b8
         pos = pos + 2
      case("b")
         okay = pos == prev
         base = b2
         pos = pos + 2
      case(char_kind%space, char_kind%tab, char_kind%newline, char_kind%carriage_return, &
         & char_kind%hash, char_kind%rbrace, char_kind%rbracket, char_kind%comma)
         token = toml_token(token_kind%int, prev, pos)
         return
      case default
         do while(verify(peek(lexer, pos), terminated) > 0)
            pos = pos + 1
         end do
         token = toml_token(token_kind%invalid, prev, pos-1)
         return
      end select
   end if


   do while(pos <= size(lexer%chunk))
      ch = peek(lexer, pos)
      if (ch == "_") then
         if (underscore) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         underscore = .true.
         pos = pos + 1
         cycle
      end if

      if (verify(ch, toml_base(base)) == 0) then
         pos = pos + 1
         underscore = .false.
         cycle
      end if

      okay = okay .and. verify(ch, terminated) == 0
      exit
   end do

   okay = .not.underscore .and. okay
   token = toml_token(merge(token_kind%int, token_kind%invalid, okay), prev, pos-1)
end subroutine next_integer

subroutine next_float(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   integer :: prev, pos
   logical :: plus_minus, underscore, point, expo, okay, zero, first
   character(1, tfc) :: ch
   integer, parameter :: offset(*) = [0, 1, 2]
   character(1, tfc), parameter :: nan(3) = ["n", "a", "n"], inf(3) = ["i", "n", "f"]

   prev = lexer%pos
   pos = lexer%pos
   point = .false.
   expo = .false.
   zero = .false.
   first = .true.
   underscore = .true.
   plus_minus = any(match(lexer, pos, ["+", "-"]))
   if (plus_minus) pos = pos + 1

   if (match_all(lexer, pos+offset, nan) .and. &
      & verify(peek(lexer, pos+3), terminated) == 0) then
      token = toml_token(token_kind%float, prev, pos+2)
      return
   end if

   if (match_all(lexer, pos+offset, inf) .and. &
      & verify(peek(lexer, pos+3), terminated) == 0) then
      token = toml_token(token_kind%float, prev, pos+2)
      return
   end if

   do while(pos <= size(lexer%chunk))
      ch = peek(lexer, pos)
      if (ch == "_") then
         if (underscore) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         underscore = .true.
         pos = pos + 1
         cycle
      end if

      if (ch == ".") then
         if (point .or. expo .or. underscore) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         zero = .false.
         underscore = .true.
         point = .true.
         pos = pos + 1
         cycle
      end if

      if (ch == "e" .or. ch == "E") then
         if (expo .or. underscore) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         zero = .false.
         underscore = .true.
         expo = .true.
         pos = pos + 1
         cycle
      end if

      if (ch == "+" .or. ch == "-") then
         if (.not.any(match(lexer, pos-1, ["e", "E"]))) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         underscore = .true.
         pos = pos + 1
         cycle
      end if

      if (verify(ch, "0123456789") == 0) then
         if (zero) then
            token = toml_token(token_kind%invalid, prev, pos)
            return
         end if
         zero = first .and. ch == "0"
         first = .false.
         pos = pos + 1
         underscore = .false.
         cycle
      end if

      exit
   end do

   okay = .not.underscore .and. (expo .or. point)
   token = toml_token(merge(token_kind%float, token_kind%invalid, okay), prev, pos-1)
end subroutine next_float

!> Find the next datetime expression
subroutine next_datetime(lexer, token)
   !> Instance of the lexer
   type(toml_lexer), intent(inout) :: lexer
   !> Current lexeme
   type(toml_token), intent(inout) :: token

   logical :: has_date, has_time, has_millisec, has_local, okay
   integer :: prev, pos, it
   integer, parameter :: offset(*) = [(it, it = 0, 10)], &
      & offset_date = 10, offset_time = 8, offset_local = 6
   character(*, tfc), parameter :: num = "0123456789"

   prev = lexer%pos
   pos = lexer%pos

   has_date = valid_date(peek(lexer, pos+offset(:offset_date)))
   if (has_date) then
      if (verify(peek(lexer, pos+offset_date), "Tt ") == 0 &
         & .and. pos + offset_date < size(lexer%chunk) &
         & .and. verify(peek(lexer, pos+offset_date+1), num) == 0) then
         pos = pos + offset_date + 1
      end if
   end if

   has_time = valid_time(peek(lexer, pos+offset(:offset_time)))
   if (has_time) then
      pos = pos + offset_time - 1
      if (match(lexer, pos+1, char_kind%dot)) then
         it = 1
         do while(verify(peek(lexer, pos+it+1), num) == 0)
            it = it + 1
         end do
         has_millisec = it > 1
         if (.not.has_millisec) then
            token = toml_token(token_kind%invalid, prev, prev)
            return
         end if

         pos = pos + it
      end if

      has_local = valid_local(peek(lexer, pos+offset(:offset_local)+1))
      if (has_local) then
         if (.not.has_date) then
            token = toml_token(token_kind%invalid, prev, prev)
            return
         end if
         pos = pos + offset_local
      else if (verify(peek(lexer, pos+1), "zZ") == 0) then
         pos = pos + 1
      end if
   end if

   if (.not.(has_time.or.has_date)) then
      token = toml_token(token_kind%invalid, prev, prev)
      return
   end if

   if (.not.has_time.and.has_date) pos = pos + offset_date - 1
   okay = verify(peek(lexer, pos+1), terminated) == 0 .and. pos <= size(lexer%chunk)
   token = toml_token(merge(token_kind%datetime, token_kind%invalid, okay), prev, pos)
end subroutine next_datetime

!> Validate a string as date
pure function valid_date(string) result(valid)
   !> Input string, 10 characters
   character(1, tfc), intent(in) :: string(:)
   !> Valid date
   logical :: valid

   integer :: it, val
   character(*, tfc), parameter :: num = "0123456789"
   integer :: year, month, day, mday
   logical :: leap

   valid = .false.
   if (any(string([5, 8]) /= "-")) return

   year = 0
   do it = 1, 4
      val = scan(num, string(it)) - 1
      if (val < 0) return
      year = year * 10 + val
   end do

   month = 0
   do it = 6, 7
      val = scan(num, string(it)) - 1
      if (val < 0) return
      month = month * 10 + val
   end do

   day = 0
   do it = 9, 10
      val = scan(num, string(it)) - 1
      if (val < 0) return
      day = day * 10 + val
   end do

   mday = 0
   select case(month)
   case(1, 3, 5, 7, 8, 10, 12)
      mday = 31
   case(2)
      leap = mod(year, 4) == 0 .and. (mod(year, 100) /= 0 .or. mod(year, 400) == 0)
      mday = merge(29, 28, leap)
   case(4, 6, 9, 11)
      mday = 30
   end select
   valid = day >= 1 .and. day <= mday
end function valid_date


!> Validate a string as time
function valid_time(string) result(valid)
   !> Input string, 8 characters
   character(1, tfc), intent(in) :: string(:)
   !> Valid time
   logical :: valid

   integer :: it, val
   character(*, tfc), parameter :: num = "0123456789"
   integer :: hour, minute, second

   valid = .false.
   if (any(string([3, 6]) /= ":")) return

   hour = 0
   do it = 1, 2
      val = scan(num, string(it)) - 1
      if (val < 0) return
      hour = hour * 10 + val
   end do

   minute = 0
   do it = 4, 5
      val = scan(num, string(it)) - 1
      if (val < 0) return
      minute = minute * 10 + val
   end do

   second = 0
   do it = 7, 8
      val = scan(num, string(it)) - 1
      if (val < 0) return
      second = second * 10 + val
   end do

   valid = second >= 0 .and. second < 60 &
      & .and. minute >= 0 .and. minute < 60 &
      & .and. hour >= 0 .and. hour < 24
end function valid_time


!> Validate a string as timezone
function valid_local(string) result(valid)
   !> Input string, 6 characters
   character(1, tfc), intent(in) :: string(:)
   !> Valid timezone
   logical :: valid

   integer :: it, val
   character(*, tfc), parameter :: num = "0123456789"
   integer :: hour, minute

   valid = .false.
   if (string(4) /= ":" .or. all(string(1) /= ["+", "-"])) return

   hour = 0
   do it = 2, 3
      val = scan(num, string(it)) - 1
      if (val < 0) return
      hour = hour * 10 + val
   end do

   minute = 0
   do it = 5, 6
      val = scan(num, string(it)) - 1
      if (val < 0) return
      minute = minute * 10 + val
   end do

   valid = minute >= 0 .and. minute < 60 &
      & .and. hour >= 0 .and. hour < 24
end function valid_local


!> Show current character
elemental function peek(lexer, pos) result(ch)
   !> Instance of the lexer
   type(toml_lexer), intent(in) :: lexer
   !> Position to fetch character from
   integer, intent(in) :: pos
   !> Character found
   character(1, tfc) :: ch

   if (pos <= size(lexer%chunk)) then
      ch = lexer%chunk(pos)
   else
      ch = char_kind%space
   end if
end function peek

!> Compare a character
elemental function match(lexer, pos, kind)
   !> Instance of the lexer
   type(toml_lexer), intent(in) :: lexer
   !> Position to fetch character from
   integer, intent(in) :: pos
   !> Character to compare against
   character(1, tfc), intent(in) :: kind
   !> Characters match
   logical :: match

   match = peek(lexer, pos) == kind
end function match

!> Compare a set of characters
pure function match_all(lexer, pos, kind) result(match)
   !> Instance of the lexer
   type(toml_lexer), intent(in) :: lexer
   !> Position to fetch character from
   integer, intent(in) :: pos(:)
   !> Character to compare against
   character(1, tfc), intent(in) :: kind(:)
   !> Characters match
   logical :: match

   match = all(peek(lexer, pos) == kind)
end function match_all

pure function strstr(string, pattern) result(res)
   character(1, tfc), intent(in) :: string(:)
   character(1, tfc), intent(in) :: pattern(:)
   integer :: lps_array(size(pattern))
   integer :: res, s_i, p_i, length_string, length_pattern
   res = 0
   length_string = size(string)
   length_pattern = size(pattern)

   if (length_pattern > 0 .and. length_pattern <= length_string) then
      lps_array = compute_lps(pattern)

      s_i = 1
      p_i = 1
      do while(s_i <= length_string)
         if (string(s_i) == pattern(p_i)) then
            if (p_i == length_pattern) then
               res = s_i - length_pattern + 1
               exit
            end if
            s_i = s_i + 1
            p_i = p_i + 1
         else if (p_i > 1) then
            p_i = lps_array(p_i - 1) + 1
         else
            s_i = s_i + 1
         end if
      end do
   end if

contains

   pure function compute_lps(string) result(lps_array)
      character(1, tfc), intent(in) :: string(:)
      integer :: lps_array(size(string))
      integer :: i, j, length_string

      length_string = size(string)

      if (length_string > 0) then
         lps_array(1) = 0

         i = 2
         j = 1
         do while (i <= length_string)
            if (string(j) == string(i)) then
               lps_array(i) = j
               i = i + 1
               j = j + 1
            else if (j > 1) then
               j = lps_array(j - 1) + 1
            else
               lps_array(i) = 0
               i = i + 1
            end if
         end do
      end if

   end function compute_lps

end function strstr

subroutine extract_raw(lexer, token, string)
   class(toml_lexer), intent(in) :: lexer
   type(toml_token), intent(in) :: token
   character(len=:), allocatable, intent(out) :: string

   integer :: length

   length = token%last - token%first + 1
   allocate(character(length)::string)
   string = transfer(lexer%chunk(token%first:token%last), string)
end subroutine extract_raw

subroutine extract_string(lexer, token, string)
   class(toml_lexer), intent(in) :: lexer
   type(toml_token), intent(in) :: token
   character(len=:), allocatable, intent(out) :: string

   integer :: it, length
   logical :: escape
   character(1, tfc) :: ch

   length = token%last - token%first + 1

   select case(token%kind)
   case(token_kind%string)
      string = ""
      escape = .false.
      do it = token%first + 1, token%last - 1
         ch = lexer%chunk(it)
         if (escape) then
            escape = .false.
            select case(ch)
            case("""", "\");  string = string // ch
            case("b"); string = string // TOML_BACKSPACE
            case("t"); string = string // TOML_TABULATOR
            case("n"); string = string // TOML_NEWLINE
            case("r"); string = string // TOML_CARRIAGE_RETURN
            case("f"); string = string // TOML_FORMFEED
            case("u"); string = string // "\u"  ! FIXME
            case("U"); string = string // "\U"  ! FIXME
            end select
            cycle
         end if
         escape = ch == char_kind%backslash
         if (.not.escape) string = string // ch
      end do
   case(token_kind%mstring)
      string = ""
      escape = .false.
      do it = token%first + 3, token%last - 3
         ch = lexer%chunk(it)
         if (escape) then
            escape = .false.
            select case(ch)
            case("""", "\");  string = string // ch
            case("b"); string = string // TOML_BACKSPACE
            case("t"); string = string // TOML_TABULATOR
            case("n"); string = string // TOML_NEWLINE
            case("r"); string = string // TOML_CARRIAGE_RETURN
            case("f"); string = string // TOML_FORMFEED
            case("u"); string = string // "\u"  ! FIXME
            case("U"); string = string // "\U"  ! FIXME
            case(char_kind%space, char_kind%tab, char_kind%carriage_return)
               escape = .true.
            case(char_kind%newline)
               continue
            end select
            cycle
         end if
         escape = ch == char_kind%backslash
         if (.not.escape) string = string // ch
      end do
   case(token_kind%literal)
      allocate(character(length - 2)::string)
      string = transfer(lexer%chunk(token%first+1:token%last-1), string)
   case(token_kind%mliteral)
      allocate(character(length - 6)::string)
      string = transfer(lexer%chunk(token%first+3:token%last-3), string)
   case(token_kind%keypath)
      allocate(character(length)::string)
      string = transfer(lexer%chunk(token%first:token%last), string)
   end select

end subroutine extract_string


subroutine extract_integer(lexer, token, val)
   class(toml_lexer), intent(in) :: lexer
   type(toml_token), intent(in) :: token
   integer(tfi), intent(out) :: val

   integer :: first, base, it, tmp
   character(1, tfc) :: ch
   character(*, tfc), parameter :: num = "0123456789abcdef"

   if (token%kind /= token_kind%int) return

   val = 0
   base = 10
   first = token%first

   if (any(peek(lexer, first) == ["+", "-"])) first = first + 1

   if (peek(lexer, first) == "0") then
      select case(peek(lexer, first + 1))
      case("x")
         first = first + 2
         base = 16
      case("o")
         first = first + 2
         base = 8
      case("b")
         first = first + 2
         base = 2
      case default
         return
      end select
   end if

   do it = first, token%last
      ch = peek(lexer, it)
      if ("A" <= ch .and. ch <= "Z") ch = achar(iachar(ch) - iachar("A") + iachar("a"))
      tmp = scan(num(:base), ch) - 1
      if (tmp < 0) cycle
      val = val * base + tmp
   end do

   if (match(lexer, token%first, char_kind%minus)) val = -val
end subroutine extract_integer


subroutine extract_float(lexer, token, val)
   use, intrinsic :: ieee_arithmetic, only : ieee_value, &
      & ieee_positive_inf, ieee_negative_inf, ieee_quiet_nan
   class(toml_lexer), intent(in) :: lexer
   type(toml_token), intent(in) :: token
   real(tfr), intent(out) :: val

   integer :: first, it, ic
   character(len=token%last - token%first + 1) :: buffer
   character(1, tfc) :: ch

   if (token%kind /= token_kind%float) return

   first = token%first

   if (any(peek(lexer, first) == ["+", "-"])) first = first + 1

   if (match(lexer, first, "n")) then
      val = ieee_value(val, ieee_quiet_nan)
      return
   end if

   if (match(lexer, first, "i")) then
      if (match(lexer, token%first, char_kind%minus)) then
         val = ieee_value(val, ieee_negative_inf)
      else
         val = ieee_value(val, ieee_positive_inf)
      end if
      return
   end if

!   ival = 0
!   idot = 0
!
!   do it = first, token%last
!      ch = peek(lexer, it)
!      if (any(ch == [".", "e", "E"])) exit
!      tmp = scan(num(:base), ch) - 1
!      if (tmp < 0) cycle
!      ival = ival * base + tmp
!   end do
!   first = it
!
!   if (ch == ".") then
!      idot = 0
!      do it = first, token%last
!         ch = peek(lexer, it)
!         if (any(ch == ["e", "E"])) exit
!         tmp = scan(num(:base), ch) - 1
!         if (tmp < 0) cycle
!         idot = idot + 1
!         ival = ival * base + tmp
!      end do
!      first = it
!   end if
!
!   expo = 0
!   if (any(ch == ["e", "E"])) then
!      first = first + 1
!      do it = first, token%last
!         ch = peek(lexer, it)
!         tmp = scan(num(:base), ch) - 1
!         if (tmp < 0) cycle
!         expo = expo * base + tmp
!      end do
!      if (match(lexer, first, char_kind%minus)) expo = -expo
!   end if
!   expo = expo - idot
!   val = ival * 10.0_tfr ** expo  ! FIXME
!
!   if (match(lexer, token%first, char_kind%minus)) val = -val

   ic = 0
   do it = token%first, token%last
      ch = peek(lexer, it)
      if (ch == "_") cycle
      ic = ic + 1
      buffer(ic:ic) = ch
   end do

   read(buffer(:ic), *, iostat=it) val
end subroutine extract_float


subroutine extract_bool(lexer, token, val)
   class(toml_lexer), intent(in) :: lexer
   type(toml_token), intent(in) :: token
   logical, intent(out) :: val

   if (token%kind /= token_kind%bool) return

   val = (token%last - token%first + 1) == 4
end subroutine extract_bool


subroutine extract_datetime(lexer, token, val)
   class(toml_lexer), intent(in) :: lexer
   type(toml_token), intent(in) :: token
   type(toml_datetime), intent(out) :: val

   type(toml_date) :: date
   type(toml_time) :: time

   integer :: it, tmp, first
   character(*, tfc), parameter :: num = "0123456789"

   if (token%kind /= token_kind%datetime) return
   first = token%first - 1

   if (all(peek(lexer, first + [5, 8]) == "-")) then
      date%year = 0
      do it = first + 1, first + 4
         tmp = scan(num, peek(lexer, it)) - 1
         if (tmp < 0) exit
         date%year = date%year * 10 + tmp
      end do

      date%month = 0
      do it = first + 6, first + 7
         tmp = scan(num, peek(lexer, it)) - 1
         if (tmp < 0) exit
         date%month = date%month * 10 + tmp
      end do

      date%day = 0
      do it = first + 9, first + 10
         tmp = scan(num, peek(lexer, it)) - 1
         if (tmp < 0) exit
         date%day = date%day * 10 + tmp
      end do

      first = first + 11
      val%date = date
   end if

   if (all(peek(lexer, first + [3, 6]) == ":") .and. first < token%last) then
      time%hour = 0
      do it = first + 1, first + 2
         tmp = scan(num, peek(lexer, it)) - 1
         if (tmp < 0) exit
         time%hour = time%hour * 10 + tmp
      end do

      time%minute = 0
      do it = first + 4, first + 5
         tmp = scan(num, peek(lexer, it)) - 1
         if (tmp < 0) exit
         time%minute = time%minute * 10 + tmp
      end do

      time%second = 0
      do it = first + 7, first + 8
         tmp = scan(num, peek(lexer, it)) - 1
         if (tmp < 0) exit
         time%second = time%second * 10 + tmp
      end do

      first = first + 8
      if (peek(lexer, first + 1) == ".") then
         allocate(time%millisec)
         time%millisec = 0
         do it = first + 2, token%last
            tmp = scan(num, peek(lexer, it)) - 1
            if (tmp < 0) exit
            time%millisec = time%millisec * 10 + tmp
         end do
         first = it - 1
      end if

      if (first < token%last) then
         time%zone = ""
         do it = first + 1, token%last
            time%zone = time%zone // peek(lexer, first + 1)
         end do
         if (time%zone == "z") time%zone = "Z"
      end if
      val%time = time
   end if

end subroutine extract_datetime


pure subroutine push_back(lexer, scope, token)
   type(toml_lexer), intent(inout) :: lexer
   integer, intent(in) :: scope
   integer, intent(in) :: token

   lexer%top = lexer%top + 1
   if (lexer%top > size(lexer%stack)) call resize(lexer%stack)
   lexer%stack(lexer%top) = stack_item(scope, token)
end subroutine push_back


subroutine pop(lexer, scope)
   type(toml_lexer), intent(inout) :: lexer
   integer, intent(in) :: scope

   if (lexer%top > 0) then
      if (lexer%stack(lexer%top)%scope == scope) lexer%top = lexer%top - 1
   end if
end subroutine pop


pure function view_scope(lexer) result(scope)
   type(toml_lexer), intent(in) :: lexer
   integer :: scope

   if (lexer%top > 0) then
      scope = lexer%stack(lexer%top)%scope
   else
      scope = lexer_scope%table
   end if
end function view_scope


!> Reallocate list of integers
pure subroutine resize_scope(var, n)
   !> Instance of the array to be resized
   type(stack_item), allocatable, intent(inout) :: var(:)
   !> Dimension of the final array size
   integer, intent(in), optional :: n

   type(stack_item), allocatable :: tmp(:)
   integer :: this_size, new_size
   integer, parameter :: initial_size = 8

   if (allocated(var)) then
      this_size = size(var, 1)
      call move_alloc(var, tmp)
   else
      this_size = initial_size
   end if

   if (present(n)) then
      new_size = n
   else
      new_size = this_size + this_size/2 + 1
   end if

   allocate(var(new_size))

   if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(var, 1))
      var(:this_size) = tmp(:this_size)
      deallocate(tmp)
   end if

end subroutine resize_scope


end module tomlf_de_lexer
