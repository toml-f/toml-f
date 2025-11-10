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

!> Defines the abstract base class which is implemented by the TOML lexer.
module tomlf_de_abc
   use tomlf_constants, only : tfc, tfi, tfr
   use tomlf_datetime, only : toml_datetime
   use tomlf_de_token, only : toml_token
   implicit none
   private

   public :: abstract_lexer


   !> Abstract base class for TOML lexers.
   type, abstract :: abstract_lexer
   contains
      !> Obtain the next token
      procedure(next), deferred :: next
      !> Extract a token
      generic :: extract => &
         & extract_string, extract_integer, extract_float, extract_bool, extract_datetime
      !> Extract a string from a token
      procedure(extract_string), deferred :: extract_string
      !> Extract an integer from a token
      procedure(extract_integer), deferred :: extract_integer
      !> Extract a float from a token
      procedure(extract_float), deferred :: extract_float
      !> Extract a boolean from a token
      procedure(extract_bool), deferred :: extract_bool
      !> Extract a timestamp from a token
      procedure(extract_datetime), deferred :: extract_datetime
      !> Get information about the source
      procedure(get_info), deferred :: get_info
   end type abstract_lexer


   abstract interface
      !> Advance the lexer to the next token.
      subroutine next(lexer, token)
         import :: abstract_lexer, toml_token
         !> Instance of the lexer
         class(abstract_lexer), intent(inout) :: lexer
         !> Current lexeme
         type(toml_token), intent(inout) :: token
      end subroutine next

      !> Extract string value of token, works for keypath, string, multiline string, literal,
      !> and mulitline literal tokens.
      subroutine extract_string(lexer, token, string)
         import :: abstract_lexer, toml_token, tfc
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Token to extract string value from
         type(toml_token), intent(in) :: token
         !> String value of token
         character(:, tfc), allocatable, intent(out) :: string
      end subroutine extract_string

      !> Extract integer value of token
      subroutine extract_integer(lexer, token, val)
         import :: abstract_lexer, toml_token, tfi
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Token to extract integer value from
         type(toml_token), intent(in) :: token
         !> Integer value of token
         integer(tfi), intent(out) :: val
      end subroutine extract_integer

      !> Extract floating point value of token
      subroutine extract_float(lexer, token, val)
         import :: abstract_lexer, toml_token, tfr
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Token to extract floating point value from
         type(toml_token), intent(in) :: token
         !> Floating point value of token
         real(tfr), intent(out) :: val
      end subroutine extract_float

      !> Extract boolean value of token
      subroutine extract_bool(lexer, token, val)
         import :: abstract_lexer, toml_token
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Token to extract boolean value from
         type(toml_token), intent(in) :: token
         !> Boolean value of token
         logical, intent(out) :: val
      end subroutine extract_bool

      !> Extract datetime value of token
      subroutine extract_datetime(lexer, token, val)
         import :: abstract_lexer, toml_token, toml_datetime
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Token to extract datetime value from
         type(toml_token), intent(in) :: token
         !> Datetime value of token
         type(toml_datetime), intent(out) :: val
      end subroutine extract_datetime

      !> Extract information about the source
      subroutine get_info(lexer, meta, output)
         import :: abstract_lexer, tfc
         !> Instance of the lexer
         class(abstract_lexer), intent(in) :: lexer
         !> Query about the source
         character(*, tfc), intent(in) :: meta
         !> Metadata about the source
         character(:, tfc), allocatable, intent(out) :: output
      end subroutine get_info
   end interface

end module tomlf_de_abc
