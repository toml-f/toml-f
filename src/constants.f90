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

module tomlf_constants
   use, intrinsic :: iso_fortran_env, only : output_unit
   implicit none
   private

   !> Single precision real numbers
   integer, public, parameter :: tf_sp = selected_real_kind(6)

   !> Double precision real numbers
   integer, public, parameter :: tf_dp = selected_real_kind(15)

   !> Char length for integers
   integer, public, parameter :: tf_i1 = selected_int_kind(2)

   !> Short length for integers
   integer, public, parameter :: tf_i2 = selected_int_kind(4)

   !> Length of default integers
   integer, public, parameter :: tf_i4 = selected_int_kind(9)

   !> Long length for integers
   integer, public, parameter :: tf_i8 = selected_int_kind(18)


   !> Default character kind
   integer, public, parameter :: tfc = selected_char_kind('DEFAULT')

   !> Default float precision, IEEE 754 binary64 values expected
   integer, public, parameter :: tfr = tf_dp

   !> Default integer precision, 64 bit (signed long) range expected
   integer, public, parameter :: tfi = tf_i8

   !> Default output channel
   integer, public, parameter :: tfout = output_unit


   !> Possible escape characters in TOML
   type :: enum_escape

      !> Backslash is used to escape other characters
      character(kind=tfc, len=1) :: backslash = tfc_'\'

      !> Double quotes signal strings with escape characters enabled
      character(kind=tfc, len=1) :: dquote = tfc_'"'

      !> Single quotes signal strings without escape characters enabled
      character(kind=tfc, len=1) :: squote = tfc_''''

      !> Newline character
      character(kind=tfc, len=1) :: newline = achar(10, kind=tfc)

      !> Formfeed character is allowed in strings
      character(kind=tfc, len=1) :: formfeed = achar(12, kind=tfc)

      !> Carriage return is allowed as part of the newline and in strings
      character(kind=tfc, len=1) :: carriage_return = achar(13, kind=tfc)

      !> Backspace is allowed in strings
      character(kind=tfc, len=1) :: bspace = achar(8, kind=tfc)

      !> Tabulators are allowed as whitespace and in strings
      character(kind=tfc, len=1) :: tabulator = achar(9, kind=tfc)

   end type enum_escape

   !> Actual enumerator with TOML escape characters
   type(enum_escape), public, parameter :: toml_escape = enum_escape()


   !> Possible kinds of TOML values in key-value pairs
   type :: enum_type

      !> Invalid type
      integer :: invalid = 100

      !> String type
      integer :: string = 101

      !> Boolean type
      integer :: boolean = 102

      !> Integer type
      integer :: int = 103

      !> Float type
      integer :: float = 104

      !> Datetime type
      integer :: datetime = 105

   end type enum_type

   !> Actual enumerator with TOML value types
   type(enum_type), public, parameter :: toml_type = enum_type()


   !> Single quotes denote literal strings
   character(kind=tfc, len=*), public, parameter :: TOML_SQUOTE = "'"
   !> Double quotes denote strings (with escape character possible)
   character(kind=tfc, len=*), public, parameter :: TOML_DQUOTE = '"'
   character(kind=tfc, len=*), public, parameter :: TOML_NEWLINE = new_line('a') ! \n
   character(kind=tfc, len=*), public, parameter :: TOML_TABULATOR = achar(9) ! \t
   character(kind=tfc, len=*), public, parameter :: TOML_FORMFEED = achar(12) ! \f
   character(kind=tfc, len=*), public, parameter :: TOML_CARRIAGE_RETURN = achar(13) ! \r
   character(kind=tfc, len=*), public, parameter :: TOML_BACKSPACE = achar(8) ! \b
   character(kind=tfc, len=*), public, parameter :: TOML_LOWERCASE = &
      & 'abcdefghijklmnopqrstuvwxyz'
   character(kind=tfc, len=*), public, parameter :: TOML_UPPERCASE = &
      & 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   character(kind=tfc, len=*), public, parameter :: TOML_LETTERS = &
      & TOML_LOWERCASE//TOML_UPPERCASE
   !> Whitespace in TOML are blanks and tabs.
   character(kind=tfc, len=*), public, parameter :: TOML_WHITESPACE = &
      & ' '//toml_escape%tabulator
   character(kind=tfc, len=*), public, parameter :: TOML_DIGITS = '0123456789'
   character(kind=tfc, len=*), public, parameter :: TOML_BINDIGITS = &
      & '01'
   character(kind=tfc, len=*), public, parameter :: TOML_OCTDIGITS = &
      & '01234567'
   character(kind=tfc, len=*), public, parameter :: TOML_HEXDIGITS = &
      & '0123456789ABCDEFabcdef'
   character(kind=tfc, len=*), public, parameter :: TOML_TIMESTAMP = &
      & TOML_DIGITS//'.:+-T Zz'
   !> Allowed characters in TOML bare keys.
   character(kind=tfc, len=*), public, parameter :: TOML_BAREKEY = &
      & TOML_LETTERS//TOML_DIGITS//'_-'
   character(kind=tfc, len=*), public, parameter :: TOML_LITERALS = &
      & TOML_LETTERS//TOML_DIGITS//'_-+.'

end module tomlf_constants
