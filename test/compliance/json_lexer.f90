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

module tftest_json_lexer
   use tomlf_constants, only : tfc, tfi, tfr, TOML_BACKSPACE, TOML_TABULATOR, TOML_NEWLINE, &
      & TOML_CARRIAGE_RETURN, TOML_FORMFEED
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time
   use tomlf_de_abc, only : abstract_lexer
   use tomlf_de_context, only : toml_context
   use tomlf_de_token, only : toml_token, stringify, token_kind
   use tomlf_error, only : toml_error, toml_stat, make_error
   use tomlf_utils, only : read_whole_file, read_whole_line
   implicit none
   private

   public :: json_lexer
   public :: new_lexer_from_file, new_lexer_from_unit, new_lexer_from_string


   !> Tokenizer for JSON documents
   type, extends(abstract_lexer) :: json_lexer
      !> Name of the source file, used for error reporting
      character(len=:), allocatable :: filename
      !> Current internal position in the source chunk
      integer :: pos = 0
      !> Current source chunk, for convenience stored as character array rather than string
      character(:, tfc), allocatable :: chunk
   contains
      !> Obtain the next token
      procedure :: next
      !> Extract a string from a token
      procedure :: extract_string
      !> Extract an integer from a token
      procedure :: extract_integer
      !> Extract a float from a token
      procedure :: extract_float
      !> Extract a boolean from a token
      procedure :: extract_bool
      !> Extract a timestamp from a token
      procedure :: extract_datetime
      !> Extract the raw value of the token
      procedure :: extract_raw
      !> Get information about source
      procedure :: get_info
   end type json_lexer

   character(*, tfc), parameter :: terminated = &
      & " "//TOML_TABULATOR//TOML_NEWLINE//TOML_CARRIAGE_RETURN//"{}[],:"

contains

!> Create a new instance of a lexer by reading from a file
subroutine new_lexer_from_file(lexer, filename, error)
   !> Instance of the lexer
   type(json_lexer), intent(out) :: lexer
   !> Name of the file to read from
   character(len=*), intent(in) :: filename
   !> Error code
   type(toml_error), allocatable, intent(out) :: error

   integer :: stat

   lexer%pos = 0
   lexer%filename = filename
   call read_whole_file(filename, lexer%chunk, stat)
   lexer%chunk = """_"":" // lexer%chunk

   if (stat /= 0) then
      call make_error(error, "Could not open file '"//filename//"'")
   end if
end subroutine new_lexer_from_file

!> Create a new instance of a lexer by reading from a unit.
!>
!> Currently, only sequential access units can be processed by this constructor.
subroutine new_lexer_from_unit(lexer, io, error)
   !> Instance of the lexer
   type(json_lexer), intent(out) :: lexer
   !> Unit to read from
   integer, intent(in) :: io
   !> Error code
   type(toml_error), allocatable, intent(out) :: error

   character(:, tfc), allocatable :: source, line
   integer, parameter :: bufsize = 512
   character(bufsize, tfc) :: filename, mode
   integer :: stat

   inquire(unit=io, access=mode, name=filename)
   select case(trim(mode))
   case default
      stat = 1

   case("sequential", "SEQUENTIAL")
      allocate(character(0) :: source)
      do 
         call read_whole_line(io, line, stat)
         if (stat > 0) exit
         source = source // line // TOML_NEWLINE
         if (stat < 0) then
            if (is_iostat_end(stat)) stat = 0
            exit
         end if
      end do
      call new_lexer_from_string(lexer, source)
   end select
   if (len_trim(filename) > 0) lexer%filename = trim(filename)

   if (stat /= 0) then
      call make_error(error, "Failed to read from unit")
   end if
end subroutine new_lexer_from_unit

!> Create a new instance of a lexer by reading from a string.
subroutine new_lexer_from_string(lexer, string)
   !> Instance of the lexer
   type(json_lexer), intent(out) :: lexer
   !> String to read from
   character(len=*), intent(in) :: string

   integer :: length
   character(1, tfc) :: ch

   length = len(string)
   lexer%pos = 0
   lexer%chunk = """_"":" // string
end subroutine new_lexer_from_string

!> Advance to the next token in the lexer
subroutine next(lexer, token)
   !> Instance of the lexer
   class(json_lexer), intent(inout) :: lexer
   !> Current token
   type(toml_token), intent(inout) :: token

   call next_token(lexer, token)
end subroutine next

!> Actually generate the next token, unbuffered version
subroutine next_token(lexer, token)
   !> Instance of the lexer
   class(json_lexer), intent(inout) :: lexer
   !> Current token
   type(toml_token), intent(inout) :: token

   integer :: prev, pos

   ! Consume current token
   lexer%pos = lexer%pos + token%last - token%first + 1
   prev = lexer%pos
   pos = lexer%pos

   ! If lexer is exhausted, return EOF as early as possible
   if (pos > len(lexer%chunk)) then
      token = toml_token(token_kind%eof, prev, pos)
      return
   end if

   select case(lexer%chunk(pos:pos))
   case(tfc_" ", TOML_TABULATOR, TOML_NEWLINE, TOML_CARRIAGE_RETURN)
      do while(any(lexer%chunk(pos+1:pos+1) == [tfc_" ", TOML_TABULATOR, TOML_NEWLINE, &
            & TOML_CARRIAGE_RETURN]) .and. pos < len(lexer%chunk))
         pos = pos + 1
      end do
      token = toml_token(token_kind%whitespace, prev, pos)
   case(tfc_":")
      token = toml_token(token_kind%equal, prev, pos)
   case(tfc_"{")
      token = toml_token(token_kind%lbrace, prev, pos)
   case(tfc_"}")
      token = toml_token(token_kind%rbrace, prev, pos)
   case(tfc_"[")
      token = toml_token(token_kind%lbracket, prev, pos)
   case(tfc_"]")
      token = toml_token(token_kind%rbracket, prev, pos)
   case(tfc_"""")
      call next_string(lexer, token)
   case(tfc_",")
      token = toml_token(token_kind%comma, prev, pos)
   case default
      ! If the current token is invalid, advance to the next terminator
      do while(verify(lexer%chunk(pos+1:pos+1), terminated) > 0 .and. pos < len(lexer%chunk))
         pos = pos + 1
      end do
      token = toml_token(token_kind%invalid, prev, pos)
   end select

end subroutine next_token

!> Process next string token, can produce normal string and multiline string tokens
subroutine next_string(lexer, token)
   !> Instance of the lexer
   type(json_lexer), intent(inout) :: lexer
   !> Current token
   type(toml_token), intent(inout) :: token

   character(1, tfc) :: ch
   character(*, tfc), parameter :: hex = "0123456789ABCDEFabcdef", valid_escape = "btnfr\"""
   integer :: prev, pos, expect, it
   logical :: escape, valid, space

   prev = lexer%pos
   pos = lexer%pos

   valid = .true.
   escape = .false.
   expect = 0

   do while(pos < len(lexer%chunk))
      pos = pos + 1
      ch = lexer%chunk(pos:pos)
      valid = valid .and. valid_string(ch)
      if (escape) then
         escape = .false.
         if (verify(ch, valid_escape) == 0) cycle
         if (ch == "u") then
            expect = 4
            cycle
         end if
         valid = .false.
         cycle
      end if
      if (expect > 0) then
         expect = expect - 1
         valid = valid .and. verify(ch, hex) == 0
         cycle
      end if
      escape = ch == tfc_"\"
      if (ch == tfc_"""") exit
      if (ch == TOML_NEWLINE) then
         pos = pos - 1
         valid = .false.
         exit
      end if
   end do

   valid = valid .and. lexer%chunk(pos:pos) == """" .and. pos /= prev
   token = toml_token(merge(token_kind%string, token_kind%invalid, valid), prev, pos)
end subroutine next_string

!> Validate characters in string, non-printable characters are invalid in this context
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

!> Show current character
elemental function peek(lexer, pos) result(ch)
   !> Instance of the lexer
   type(json_lexer), intent(in) :: lexer
   !> Position to fetch character from
   integer, intent(in) :: pos
   !> Character found
   character(1, tfc) :: ch

   if (pos <= len(lexer%chunk)) then
      ch = lexer%chunk(pos:pos)
   else
      ch = " "
   end if
end function peek

!> Compare a character
elemental function match(lexer, pos, kind)
   !> Instance of the lexer
   type(json_lexer), intent(in) :: lexer
   !> Position to fetch character from
   integer, intent(in) :: pos
   !> Character to compare against
   character(1, tfc), intent(in) :: kind
   !> Characters match
   logical :: match

   match = peek(lexer, pos) == kind
end function match

!> Extract raw value of token
subroutine extract_raw(lexer, token, string)
   !> Instance of the lexer
   class(json_lexer), intent(in) :: lexer
   !> Token to extract raw value from
   type(toml_token), intent(in) :: token
   !> Raw value of token
   character(len=:), allocatable, intent(out) :: string

   integer :: length

   length = token%last - token%first + 1
   allocate(character(length) :: string)
   string = lexer%chunk(token%first:token%last)
end subroutine extract_raw

!> Extract string value of token, works for keypath, string, multiline string, literal,
!> and mulitline literal tokens.
subroutine extract_string(lexer, token, string)
   !> Instance of the lexer
   class(json_lexer), intent(in) :: lexer
   !> Token to extract string value from
   type(toml_token), intent(in) :: token
   !> String value of token
   character(len=:), allocatable, intent(out) :: string

   integer :: it, length
   logical :: escape, leading_newline
   character(1, tfc) :: ch

   length = token%last - token%first + 1

   select case(token%kind)
   case(token_kind%string)
      string = ""
      escape = .false.
      do it = token%first + 1, token%last - 1
         ch = peek(lexer, it)
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
            end select
            cycle
         end if
         escape = ch == "\"
         if (.not.escape) string = string // ch
      end do
   case(token_kind%keypath)
      string = ""
   end select

end subroutine extract_string

!> Extract integer value of token
subroutine extract_integer(lexer, token, val)
   !> Instance of the lexer
   class(json_lexer), intent(in) :: lexer
   !> Token to extract integer value from
   type(toml_token), intent(in) :: token
   !> Integer value of token
   integer(tfi), intent(out) :: val

   integer :: first, base, it, tmp
   character(1, tfc) :: ch
   character(*, tfc), parameter :: num = tfc_"0123456789"

   if (token%kind /= token_kind%int) return

   val = 0
   base = 10
   first = token%first

   if (peek(lexer, first) == tfc_"-") first = first + 1
   if (peek(lexer, first) == tfc_"0") return

   do it = first, token%last
      ch = peek(lexer, it)
      tmp = scan(num, ch) - 1
      if (tmp < 0) cycle
      val = val * base + tmp
   end do

   if (match(lexer, token%first, tfc_"-")) val = -val
end subroutine extract_integer

!> Extract floating point value of token
subroutine extract_float(lexer, token, val)
   use, intrinsic :: ieee_arithmetic, only : ieee_value, &
      & ieee_positive_inf, ieee_negative_inf, ieee_quiet_nan
   !> Instance of the lexer
   class(json_lexer), intent(in) :: lexer
   !> Token to extract floating point value from
   type(toml_token), intent(in) :: token
   !> Floating point value of token
   real(tfr), intent(out) :: val

   integer :: stat

   if (token%kind /= token_kind%float) return

   read(lexer%chunk(token%first:token%last), *, iostat=stat) val
end subroutine extract_float

!> Extract boolean value of token
subroutine extract_bool(lexer, token, val)
   !> Instance of the lexer
   class(json_lexer), intent(in) :: lexer
   !> Token to extract boolean value from
   type(toml_token), intent(in) :: token
   !> Boolean value of token
   logical, intent(out) :: val

   if (token%kind /= token_kind%bool) return

   val = peek(lexer, token%first) == "t"
end subroutine extract_bool

!> Extract datetime value of token
subroutine extract_datetime(lexer, token, val)
   !> Instance of the lexer
   class(json_lexer), intent(in) :: lexer
   !> Token to extract datetime value from
   type(toml_token), intent(in) :: token
   !> Datetime value of token
   type(toml_datetime), intent(out) :: val
end subroutine extract_datetime

!> Extract information about the source
subroutine get_info(lexer, meta, output)
   !> Instance of the lexer
   class(json_lexer), intent(in) :: lexer
   !> Query about the source
   character(*, tfc), intent(in) :: meta
   !> Metadata about the source
   character(:, tfc), allocatable, intent(out) :: output

   select case(meta)
   case("source")
      output = lexer%chunk // TOML_NEWLINE
   case("filename")
      if (allocated(lexer%filename)) output = lexer%filename
   end select
end subroutine get_info

end module tftest_json_lexer
