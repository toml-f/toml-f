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

!> Implementation of a tokenizer for character variables
module tomlf_de_character
   use tomlf_constants
   use tomlf_error, only : syntax_error
   use tomlf_de_tokenizer
   use tomlf_utils
   implicit none
   private

   public :: toml_character_tokenizer, new_character_tokenizer, new


   !> Tokenizer for a sequence of characters
   type, extends(toml_tokenizer) :: toml_character_tokenizer

      !> Link to the input configuration.
      character(len=:), pointer :: conf

   contains

      !> Return next token
      procedure :: next_token

   end type toml_character_tokenizer


   interface new
      module procedure :: new_character_tokenizer
   end interface new


contains


!> Constructor for the deserializer implementation.
subroutine new_character_tokenizer(de, conf)
   type(toml_character_tokenizer), intent(out) :: de
   character(len=*), intent(in), target :: conf
   !> connect deserializer to configuration
   de%conf => conf
   de%line%ptr => conf
   de%line%num = 1
   !> first token is an artifical newline
   de%tok = new_token(toml_tokentype%newline, de%conf, 0)
end subroutine new_character_tokenizer


!> Return next token
subroutine next_token(de, dot_is_token)

   !> Instance of the tokenizer
   class(toml_character_tokenizer), intent(inout) :: de

   !> Dot should be handled as token
   logical, intent(in) :: dot_is_token

   character(len=:), pointer :: ptr
   integer :: i, skip
   if (de%finished) return
   ptr => de%tok%ptr

   !> consume token
   do i = 1, de%tok%len
      de%line%pos = de%line%pos + 1
      if (ptr(i:i) == TOML_NEWLINE) then
         de%line%ptr => ptr(min(i+1, len(ptr)):)
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
         if (dot_is_token) then
            de%tok = new_token(toml_tokentype%dot, ptr, 1)
            return
         end if
      case(','); de%tok = new_token(toml_tokentype%comma, ptr, 1); return
      case('='); de%tok = new_token(toml_tokentype%equal, ptr, 1); return
      case('{'); de%tok = new_token(toml_tokentype%lbrace, ptr, 1); return
      case('}'); de%tok = new_token(toml_tokentype%rbrace, ptr, 1); return
      case('['); de%tok = new_token(toml_tokentype%lbracket, ptr, 1); return
      case(']'); de%tok = new_token(toml_tokentype%rbracket, ptr, 1); return
      case(TOML_NEWLINE); de%tok = new_token(toml_tokentype%newline, ptr, 1); return
      case(' ', char(9));
         skip = verify(ptr, TOML_WHITESPACE)-1
         de%tok = new_token(toml_tokentype%whitespace, ptr, skip)
         return
      end select

      call scan_string(de, ptr, dot_is_token)
      return
   end do

   !> return with EOF token
   de%finished = .true.
   de%tok = new_token(toml_tokentype%newline, ptr, 0)

contains

   subroutine scan_string(de, ptr, dot_is_token)
   class(toml_character_tokenizer), intent(inout) :: de
   character(len=:), pointer, intent(inout) :: ptr
   logical, intent(in) :: dot_is_token
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
            call syntax_error(de%error, de%line, "unterminated triple-s-quote")
            return
         end if

         de%tok = new_token(toml_tokentype%string, orig, i+5)
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
               call syntax_error(de%error, de%line, "bad escape char")
               return
            end if
            if (hexreq > 0) then
               hexreq = hexreq - 1
               if (verify(ptr(i:i), TOML_HEXDIGITS) == 0) cycle
               call syntax_error(de%error, de%line, "expect hex char")
               return
            end if
            if (ptr(i:i) == TOML_DQUOTE) then
               if (qcount < 5) then
                  qcount = qcount + 1
               else
                  call syntax_error(de%error, de%line, "too many quotation marks")
                  return
               end if
            else
               if (qcount >= 3) then
                  ptr => ptr(i:)
                  exit
               end if
               qcount = 0
            end if
            if (ptr(i:i) == '\') then
               escape = .true.
               cycle
            end if
         end do
         if (qcount < 3) then
            call syntax_error(de%error, de%line, "unterminated triple-quote")
            return
         end if

         de%tok = new_token(toml_tokentype%string, orig, len(orig)-len(ptr))
         return
      end if
   end if

   if (ptr(1:1) == TOML_SQUOTE) then
      ptr => ptr(2:)
      i = index(ptr, TOML_NEWLINE)
      if (i == 0) i = len(ptr)
      i = index(ptr(:i), TOML_SQUOTE)
      if (i == 0) then
         call syntax_error(de%error, de%line, "unterminated s-quote")
         return
      end if

      de%tok = new_token(toml_tokentype%string, orig, i+1)
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

      de%tok = new_token(toml_tokentype%string, orig, len(orig)-len(ptr)+1)
      return
   end if

   if (toml_raw_verify_date(ptr) .or. toml_raw_verify_time(ptr)) then
      i = verify(ptr, TOML_TIMESTAMP)-1
      if (i < 0) i = len(ptr)
      de%tok = new_token(toml_tokentype%string, orig, i)
      return
   end if

   do i = 1, len(ptr)
      if (ptr(i:i) == '.' .and. dot_is_token) then
         ptr => ptr(i:)
         exit
      end if
      if (verify(ptr(i:i), TOML_LITERALS) == 0) cycle
      ptr => ptr(i:)
      exit
   end do

   de%tok = new_token(toml_tokentype%string, orig, len(orig) - len(ptr))

end subroutine scan_string

end subroutine next_token


!> custom constructor to get pointer assignment right
type(toml_token) function new_token(tok, ptr, len)
   integer, intent(in) :: tok
   character(len=:), pointer, intent(in) :: ptr
   integer, intent(in) :: len
   new_token%tok = tok
   new_token%ptr => ptr
   new_token%len = len
end function new_token


end module tomlf_de_character
