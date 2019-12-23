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

module tomlf08_constants
   use iso_fortran_env, only: int64, real64
   implicit none
   private :: int64, real64

   !> 64 bit (signed long) range expected
   integer, parameter :: TOML_INTEGER_KIND = int64
   !> Floats should be implemented as IEEE 754 binary64 values
   integer, parameter :: TOML_FLOAT_KIND = real64
   !> Booleans are just the tokens you're used to. Always lowercase.
   !  No word about precision...
   integer, parameter :: TOML_BOOL_KIND = TOML_INTEGER_KIND

   !> Single quotes denote literal strings
   character(len=*), parameter :: TOML_SQUOTE = "'"
   !> Double quotes denote strings (with escape character possible)
   character(len=*), parameter :: TOML_DQUOTE = '"'
   character(len=*), parameter :: TOML_NEWLINE = new_line('a')
   character(len=*), parameter :: TOML_LOWERCASE = 'abcdefghijklmnopqrstuvwxyz'
   character(len=*), parameter :: TOML_UPPERCASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   character(len=*), parameter :: TOML_LETTERS = TOML_LOWERCASE//TOML_UPPERCASE
   !> Whitespace in TOML are blanks and tabs.
   character(len=*), parameter :: TOML_WHITESPACE = ' '//char(9)
   character(len=*), parameter :: TOML_DIGITS = '0123456789'
   character(len=*), parameter :: TOML_BINDIGITS = '01'
   character(len=*), parameter :: TOML_OCTDIGITS = '01234567'
   character(len=*), parameter :: TOML_HEXDIGITS = '0123456789ABCDEFabcdef'
   character(len=*), parameter :: TOML_TIMESTAMP = TOML_DIGITS//'.:+-T Zz'
   !> Allowed characters in TOML bare keys.
   character(len=*), parameter :: TOML_BAREKEY = TOML_LETTERS//TOML_DIGITS//'_-'
   character(len=*), parameter :: TOML_LITERALS = TOML_LETTERS//TOML_DIGITS//'_-+.'

   enum, bind(C)
      enumerator :: INVALID_KIND, KEYVAL_KIND, ARRAY_KIND, TABLE_KIND
   end enum
   integer, parameter :: toml_kind_t = kind(INVALID_KIND)

   enum, bind(C)
      enumerator :: INVALID_TYPE, INTEGER_TYPE, FLOAT_TYPE, BOOL_TYPE, &
         & STRING_TYPE, TIMESTAMP_TYPE
   end enum
   integer, parameter :: toml_type_t = kind(INVALID_TYPE)

end module tomlf08_constants
