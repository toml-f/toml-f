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

module test_lexer
   use testdrive
   use tjson_lexer
   use tomlf_constants, only : tfi, tfr, nl => TOML_NEWLINE
   implicit none

   public :: collect_lexer

contains

!> Collect all exported unit tests
subroutine collect_lexer(testsuite)
   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("bool-true", bool_true), &
      & new_unittest("bool-true-or-not", bool_nottrue), &
      & new_unittest("bool-false", bool_false), &
      & new_unittest("bool-falsey", bool_falsey), &
      & new_unittest("brace", brace), &
      & new_unittest("brace-right", brace_right), &
      & new_unittest("bracket", bracket), &
      & new_unittest("bracket-left", bracket_left), &
      & new_unittest("bracket-right", bracket_right), &
      & new_unittest("colon", colon), &
      & new_unittest("empty", empty), &
      & new_unittest("equal", equal), &
      & new_unittest("float-point", float_point), &
      & new_unittest("float-exponent", float_exponent), &
      & new_unittest("float-zero", float_zero), &
      & new_unittest("float-double-point", float_double_point), &
      & new_unittest("float-double-expo", float_double_expo), &
      & new_unittest("float-invalid", float_invalid), &
      & new_unittest("integer-limits", integer_limits), &
      & new_unittest("integer-leading-zero", integer_leading_zero), &
      & new_unittest("string", string), &
      & new_unittest("string-unclosed", string_unclosed), &
      & new_unittest("string-control", string_control), &
      & new_unittest("string-escape", string_escape), &
      & new_unittest("token-integer", token_integer), &
      & new_unittest("token-float", token_float), &
      & new_unittest("token-bool", token_bool), &
      & new_unittest("whitespace-blank", whitespace_blank), &
      & new_unittest("whitespace-tab", whitespace_tab), &
      & new_unittest("whitespace-mixed", whitespace_mixed)]

end subroutine collect_lexer

subroutine empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "", &
      & [token_kind%eof])
end subroutine empty

subroutine brace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "{}", &
      & [token_kind%lbrace, token_kind%rbrace, token_kind%eof])
end subroutine brace

subroutine brace_right(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "}", &
      & [token_kind%rbrace, token_kind%eof])
end subroutine brace_right

subroutine bracket(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "[]", &
      & [token_kind%lbracket, token_kind%rbracket, token_kind%eof])
end subroutine bracket

subroutine bracket_left(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "[", &
      & [token_kind%lbracket, token_kind%eof])
end subroutine bracket_left

subroutine bracket_right(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "]", &
      & [token_kind%rbracket, token_kind%eof])
end subroutine bracket_right

subroutine dot_invalid(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, ".", &
      & [token_kind%invalid, token_kind%eof])
end subroutine dot_invalid

subroutine comma(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, ",", &
      & [token_kind%comma, token_kind%eof])
end subroutine comma

subroutine colon(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, ":", &
      & [token_kind%equal, token_kind%eof])
end subroutine colon

subroutine equal(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "=", &
      & [token_kind%invalid, token_kind%eof])
end subroutine equal

subroutine string(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """something"",""anything""", &
      & [token_kind%string, token_kind%comma, token_kind%string, token_kind%eof])
end subroutine string

subroutine string_unclosed(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """right open"//nl//"""fully closed""", &
      & [token_kind%invalid, token_kind%whitespace, token_kind%string, token_kind%eof])
end subroutine string_unclosed

subroutine string_control(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """control char "//achar(0)//""",""normal string""", &
      & [token_kind%invalid, token_kind%comma, token_kind%string, token_kind%eof])
end subroutine string_control

subroutine string_escape(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """a \b backspace character"",""a \t tab character"""//nl//&
      & """a \n new line character"",""a \f form feed character"""//nl//&
      & """a \r carriage return character"",""a \"" quote character"""//nl//&
      & """a \\ backslash character"""//nl//&
      & """not a unicode \\u escape"",""not a unicode \u005Cu escape"""//nl//&
      & """not a unicode \\u0075 escape"",""not a unicode \\\u0075 escape"""//nl//&
      & """a \u007F delete control code"",""a \u001F unit separator control code""", &
      & [token_kind%string, token_kind%comma, token_kind%string, token_kind%whitespace, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%whitespace, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%whitespace, &
      &  token_kind%string, token_kind%whitespace, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%whitespace, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%whitespace, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%eof])
end subroutine string_escape

subroutine whitespace_blank(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "    ", &
      & [token_kind%whitespace, token_kind%eof])
end subroutine whitespace_blank

subroutine whitespace_tab(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, achar(9)//achar(9), &
      & [token_kind%whitespace, token_kind%eof])
end subroutine whitespace_tab

subroutine whitespace_mixed(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, achar(9)//" "//achar(9), &
      & [token_kind%whitespace, token_kind%eof])
end subroutine whitespace_mixed

subroutine bool_false(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "false", &
      & [token_kind%bool, token_kind%eof])
end subroutine bool_false

subroutine bool_falsey(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "falsey", &
      & [token_kind%invalid, token_kind%eof])
end subroutine bool_falsey

subroutine bool_true(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "true", &
      & [token_kind%bool, token_kind%eof])
end subroutine bool_true

subroutine bool_nottrue(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "true-or-not", &
      & [token_kind%invalid, token_kind%eof])
end subroutine bool_nottrue

subroutine float_point(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "3.14,+3.14,-3.14,0.123", &
      & [token_kind%float, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%eof])
end subroutine float_point

subroutine float_exponent(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "3e2,3E2,3e-2,3E+2,3e0,3.1e2,3.1E2,-1E-1", &
      & [token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%eof])
end subroutine float_exponent

subroutine float_zero(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "0.0,+0.0,-0.0,0e0,0e00,+0e0,-0e0", &
      & [token_kind%float, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%float, token_kind%eof])
end subroutine float_zero

subroutine float_double_point(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "0..1,0.1.2", &
      & [token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%eof])
end subroutine float_double_point

subroutine float_double_expo(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1ee2,1e2e3", &
      & [token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%eof])
end subroutine float_double_expo

subroutine float_invalid(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "+1.2,1.,+1.,-1.,03.14,+03.14,-03.14", &
      & [token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%invalid, token_kind%eof])
end subroutine float_invalid

subroutine integer_limits(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "9223372036854775807,-9223372036854775808", &
      & [token_kind%int, token_kind%comma, token_kind%int, token_kind%eof])
end subroutine integer_limits

subroutine integer_leading_zero(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "001", &
      & [token_kind%invalid, token_kind%eof])
end subroutine integer_leading_zero

subroutine token_integer(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(json_lexer) :: lexer
   integer(tfi) :: val1

   call new_lexer_from_string(lexer, "1023,2047,-612")
   call lexer%extract(toml_token(token_kind%int, 1, 4), val1)
   call check(error, val1, 1023_tfi)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%int, 6, 9), val1)
   call check(error, val1, 2047_tfi)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%int, 11, 14), val1)
   call check(error, val1, -612_tfi)
   if (allocated(error)) return
end subroutine token_integer

subroutine token_float(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(json_lexer) :: lexer
   real(tfr) :: val1

   call new_lexer_from_string(lexer, "1023.0,20000.47,-6.12e-1,1e0,1e+0,1e-0")
   call lexer%extract(toml_token(token_kind%float, 1, 6), val1)
   call check(error, val1, 1023.0_tfr)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 8, 15), val1)
   call check(error, val1, 20000.47_tfr)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 17, 24), val1)
   call check(error, val1, -6.12e-1_tfr)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 26, 28), val1)
   call check(error, val1, 1.0_tfr)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 30, 33), val1)
   call check(error, val1, 1.0_tfr)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 35, 38), val1)
   call check(error, val1, 1.0_tfr)
   if (allocated(error)) return
end subroutine token_float

subroutine token_bool(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(json_lexer) :: lexer
   logical :: val

   call new_lexer_from_string(lexer, "true,false")
   call lexer%extract(toml_token(token_kind%bool, 1, 4), val)
   call check(error, val .eqv. .true.)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%bool, 6, 10), val)
   call check(error, val .eqv. .false.)
   if (allocated(error)) return
end subroutine token_bool

subroutine check_token(error, string, expected)
   use tomlf_diagnostic, only : render, toml_label, toml_level
   use tomlf_de_token, only : stringify
   use tomlf_terminal, only : toml_terminal
   !> Error handling
   type(error_type), allocatable, intent(out) :: error
   !> String to be parsed
   character(len=*), intent(in) :: string
   !> Expected token kind
   integer, intent(in) :: expected(:)

   integer :: it
   logical :: okay
   character(len=:), allocatable :: msg
   type(json_lexer) :: lexer
   type(toml_token) :: token
   type(toml_label), allocatable :: label(:)

   call new_lexer_from_string(lexer, string)
   lexer%prelude = 0

   allocate(label(0))
   do it = 1, size(expected)
      call lexer%next(token)
      okay = token%kind == expected(it)
      label = [label, toml_label(merge(toml_level%info, toml_level%error, okay), &
         &     token%first, token%last, stringify(token), .not.okay)]
      msg = render(string//nl, [label(size(label))], toml_terminal(.true.))
      call check(error, token%kind, expected(it), &
         & "Expected '"//stringify(toml_token(expected(it)))// &
         & "' but got '"//stringify(token)//"'"//nl//msg)
      if (allocated(error)) exit
   end do
   if (.not.allocated(error)) then
      msg = render(string//nl, label, toml_terminal(.true.))
      print '(a)', msg
   end if
end subroutine check_token

end module test_lexer
