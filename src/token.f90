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

!> Provides a definition for a token
module tomlf_de_token
   implicit none
   private

   public :: toml_token, stringify, token_kind, resize


   !> Possible token kinds
   type :: enum_token
      !> Invalid token found
      integer :: invalid = -1
      !> End of file
      integer :: eof = -2
      !> Unclosed group from inline table or array
      integer :: unclosed = -3
      !> Whitespace (space, tab)
      integer :: whitespace = 0
      !> Newline character (\r\n, \n)
      integer :: newline = 1
      !> Comments (#)
      integer :: comment = 2
      !> Separator in table path (.)
      integer :: dot = 3
      !> Separator in inline arrays and inline tables (,)
      integer :: comma = 4
      !> Separator in key-value pairs (=)
      integer :: equal = 5
      !> Beginning of an inline table ({)
      integer :: lbrace = 6
      !> End of an inline table (})
      integer :: rbrace = 7
      !> Beginning of an inline array or table header ([)
      integer :: lbracket = 8
      !> End of an inline array or table header (])
      integer :: rbracket = 9
      !> String literal
      integer :: string = 10
      !> String literal
      integer :: mstring = 11
      !> String literal
      integer :: literal = 12
      !> String literal
      integer :: mliteral = 13
      !> String literal
      integer :: keypath = 14
      !> Floating point value
      integer :: float = 15
      !> Integer value
      integer :: int = 16
      !> Boolean value
      integer :: bool = 17
      !> Datetime value
      integer :: datetime = 18
      !> Absence of value
      integer :: nil = 19
   end type enum_token

   !> Actual enumerator for token kinds
   type(enum_token), parameter :: token_kind = enum_token()

   !> Token containing
   type :: toml_token
      !> Kind of token
      integer :: kind = token_kind%newline
      !> Starting position of the token in character stream
      integer :: first = 0
      !> Last position of the token in character stream
      integer :: last = 0
      !> Identifier for the chunk index in case of buffered reading
      integer :: chunk = 0
   end type toml_token

   !> Reallocate a list of tokens
   interface resize
      module procedure :: resize_token
   end interface

contains

!> Reallocate list of tokens
pure subroutine resize_token(var, n)
   !> Instance of the array to be resized
   type(toml_token), allocatable, intent(inout) :: var(:)
   !> Dimension of the final array size
   integer, intent(in), optional :: n

   type(toml_token), allocatable :: tmp(:)
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

end subroutine resize_token

!> Represent a token as string
pure function stringify(token) result(str)
   !> Token to represent as string
   type(toml_token), intent(in) :: token
   !> String representation of token
   character(len=:), allocatable :: str

   select case(token%kind)
   case default; str = "unknown"
   case(token_kind%invalid); str = "invalid sequence"
   case(token_kind%eof); str = "end of file"
   case(token_kind%unclosed); str = "unclosed group"
   case(token_kind%whitespace); str = "whitespace"
   case(token_kind%comment); str = "comment"
   case(token_kind%newline); str = "newline"
   case(token_kind%dot); str = "dot"
   case(token_kind%comma); str = "comma"
   case(token_kind%equal); str = "equal"
   case(token_kind%lbrace); str = "opening brace"
   case(token_kind%rbrace); str = "closing brace"
   case(token_kind%lbracket); str = "opening bracket"
   case(token_kind%rbracket); str = "closing bracket"
   case(token_kind%string); str = "string"
   case(token_kind%mstring); str = "multiline string"
   case(token_kind%literal); str = "literal"
   case(token_kind%mliteral); str = "multiline-literal"
   case(token_kind%keypath); str = "keypath"
   case(token_kind%int); str = "integer"
   case(token_kind%float); str = "float"
   case(token_kind%bool); str = "bool"
   case(token_kind%datetime); str = "datetime"
   end select
end function stringify

end module tomlf_de_token
