! This file is part of toml-f.
!
! Copyright (C) 2019-2020 Sebastian Ehlert
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

   !> Default character kind
   integer, public, parameter :: tfc = selected_char_kind('DEFAULT')

   !> Default float precision, IEEE 754 binary64 values expected
   integer, public, parameter :: tfr = selected_real_kind(15)

   !> Default integer precision, 64 bit (signed long) range expected
   integer, public, parameter :: tfi = selected_int_kind(18)

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
