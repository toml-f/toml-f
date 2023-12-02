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

module tftest_lexer
   use testdrive
   use tomlf_constants, only : tfi, tfr, nl => TOML_NEWLINE
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time, operator(==), to_string
   use tomlf_de_lexer
   use tomlf_error, only : toml_error
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
      & new_unittest("brace-left", brace_left), &
      & new_unittest("brace-right", brace_right), &
      & new_unittest("brace-unclosed", brace_unclosed), &
      & new_unittest("bracket", bracket), &
      & new_unittest("bracket-left", bracket_left), &
      & new_unittest("bracket-right", bracket_right), &
      & new_unittest("bracket-unclosed", bracket_unclosed), &
      & new_unittest("comma", comma), &
      & new_unittest("comment", comment), &
      & new_unittest("comment-eof", comment_eof), &
      & new_unittest("datetime", datetime), &
      & new_unittest("datetime-comment", datetime_comment), &
      & new_unittest("datetime-local-date", datetime_local_date), &
      & new_unittest("datetime-local-time", datetime_local_time), &
      & new_unittest("datetime-local", datetime_local), &
      & new_unittest("datetime-milliseconds", datetime_milliseconds), &
      & new_unittest("datetime-timezone", datetime_timezone), &
      & new_unittest("datetime-hour-over", datetime_hour_over), &
      & new_unittest("datetime-mday-over", datetime_mday_over), &
      & new_unittest("datetime-mday-under", datetime_mday_under), &
      & new_unittest("datetime-mday-leap", datetime_mday_leap), &
      & new_unittest("datetime-minute-over", datetime_minute_over), &
      & new_unittest("datetime-month-over", datetime_month_over), &
      & new_unittest("datetime-month-under", datetime_month_under), &
      & new_unittest("datetime-no-leading-zeros", datetime_no_leading_zeros), &
      & new_unittest("datetime-no-seconds", datetime_no_seconds), &
      & new_unittest("datetime-no-separator", datetime_no_separator), &
      & new_unittest("datetime-trailing-separator", datetime_trailing_separator), &
      & new_unittest("datetime-local-timezone", datetime_local_timezone), &
      & new_unittest("dot-keypath", dot_keypath), &
      & new_unittest("dot-invalid", dot_invalid), &
      & new_unittest("empty", empty), &
      & new_unittest("equal", equal), &
      & new_unittest("float-nan", float_nan), &
      & new_unittest("float-inf", float_inf), &
      & new_unittest("float-point", float_point), &
      & new_unittest("float-exponent", float_exponent), &
      & new_unittest("float-underscore", float_underscore), &
      & new_unittest("float-zero", float_zero), &
      & new_unittest("float-double-point", float_double_point), &
      & new_unittest("float-double-expo", float_double_expo), &
      & new_unittest("float-dunderscored", float_dunderscored), &
      & new_unittest("float-invalid", float_invalid), &
      & new_unittest("float-leading-underscore", float_leading_underscore), &
      & new_unittest("float-trailing-underscore", float_trailing_underscore), &
      & new_unittest("integer-zero", integer_zero), &
      & new_unittest("integer-limits", integer_limits), &
      & new_unittest("integer-hexadecimal", integer_hexadecimal), &
      & new_unittest("integer-octal", integer_octal), &
      & new_unittest("integer-binary", integer_binary), &
      & new_unittest("integer-leading-zero", integer_leading_zero), &
      & new_unittest("integer-underscored", integer_underscored), &
      & new_unittest("integer-dunderscored", integer_dunderscored), &
      & new_unittest("integer-trailing_underscore", integer_trailing_underscore), &
      & new_unittest("keypath", keypath), &
      & new_unittest("keypath-dotted", keypath_dotted), &
      & new_unittest("keypath-whitespace", keypath_whitespace), &
      & new_unittest("keypath-string", keypath_string), &
      & new_unittest("keypath-inline-table", keypath_inline_table), &
      & new_unittest("keypath-inline-array", keypath_inline_array), &
      & new_unittest("keypath-inline-array-nested", keypath_inline_array_nested), &
      & new_unittest("keypath-inline-unclosed", keypath_inline_unclosed), &
      & new_unittest("keypath-invalid", keypath_invalid), &
      & new_unittest("keypath-escape", keypath_escape), &
      & new_unittest("newline-lf", newline_lf), &
      & new_unittest("newline-crlf", newline_crlf), &
      & new_unittest("newline-cr", newline_cr), &
      & new_unittest("literal", literal), &
      & new_unittest("literal-unclosed", literal_unclosed), &
      & new_unittest("literal-control", literal_control), &
      & new_unittest("literal-triple", literal_triple), &
      & new_unittest("literal-multiline", literal_multiline), &
      & new_unittest("literal-multiline-five", literal_multiline_five), &
      & new_unittest("literal-multiline-six", literal_multiline_six), &
      & new_unittest("literal-multiline-unclosed", literal_multiline_unclosed), &
      & new_unittest("string", string), &
      & new_unittest("string-unclosed", string_unclosed), &
      & new_unittest("string-control", string_control), &
      & new_unittest("string-escape", string_escape), &
      & new_unittest("string-escape-invalid", string_escape_invalid), &
      & new_unittest("string-unicode-escape", string_unicode_escape), &
      & new_unittest("string-triple", string_triple), &
      & new_unittest("string-multiline", string_multiline), &
      & new_unittest("string-multiline-unclosed1", string_multiline_unclosed1), &
      & new_unittest("string-multiline-unclosed2", string_multiline_unclosed2), &
      & new_unittest("string-multiline-unclosed3", string_multiline_unclosed3), &
      & new_unittest("string-multiline-escape", string_multiline_escape), &
      & new_unittest("token-keypath-string", token_keypath_string), &
      & new_unittest("token-keypath-mstring", token_keypath_mstring), &
      & new_unittest("token-keypath-literal", token_keypath_literal), &
      & new_unittest("token-keypath-mliteral", token_keypath_mliteral), &
      & new_unittest("token-integer", token_integer), &
      & new_unittest("token-integer-binary", token_integer_binary), &
      & new_unittest("token-integer-octal", token_integer_octal), &
      & new_unittest("token-integer-hexadecimal", token_integer_hexadecimal), &
      & new_unittest("token-float", token_float), &
      & new_unittest("token-float-exceptional", token_float_exceptional), &
      & new_unittest("token-float-fuzz", token_float_fuzz), &
      & new_unittest("token-datetime", token_datetime), &
      & new_unittest("token-string", token_string), &
      & new_unittest("token-bool", token_bool), &
      & new_unittest("whitespace-blank", whitespace_blank), &
      & new_unittest("whitespace-tab", whitespace_tab), &
      & new_unittest("whitespace-mixed", whitespace_mixed), &
      & new_unittest("lexer-from-sequential", lexer_from_sequential)]

end subroutine collect_lexer

subroutine comment(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "# comment", &
      & [token_kind%comment, token_kind%eof], .false.)
end subroutine comment

subroutine comment_eof(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "# This is a full-line comment"//nl//&
      & "key = ""value"" # This is a comment at the end of a line", &
      & [token_kind%comment, token_kind%newline, token_kind%keypath, token_kind%whitespace, &
      &  token_kind%equal, token_kind%whitespace, token_kind%string, token_kind%whitespace, &
      &  token_kind%comment, token_kind%eof], .true.)
end subroutine comment_eof

subroutine empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "", &
      & [token_kind%eof], .false.)
end subroutine empty

subroutine brace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "{}", &
      & [token_kind%lbrace, token_kind%rbrace, token_kind%eof], .false.)
end subroutine brace

subroutine brace_left(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "{", &
      & [token_kind%unclosed, token_kind%eof], .false.)
end subroutine brace_left

subroutine brace_unclosed(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "a={{{", &
      & [token_kind%keypath, token_kind%equal, token_kind%unclosed, token_kind%unclosed, &
      &  token_kind%unclosed, token_kind%eof], .true.)
end subroutine brace_unclosed

subroutine brace_right(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "}", &
      & [token_kind%rbrace, token_kind%eof], .false.)
end subroutine brace_right

subroutine bracket(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "[]", &
      & [token_kind%lbracket, token_kind%rbracket, token_kind%eof], .false.)
end subroutine bracket

subroutine bracket_left(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "[", &
      & [token_kind%lbracket, token_kind%eof], .true.)
end subroutine bracket_left

subroutine bracket_right(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "]", &
      & [token_kind%rbracket, token_kind%eof], .false.)
end subroutine bracket_right

subroutine bracket_unclosed(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "a=[", &
      & [token_kind%keypath, token_kind%equal, token_kind%unclosed, token_kind%eof], .true.)
end subroutine bracket_unclosed

subroutine dot_keypath(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, ".", &
      & [token_kind%dot, token_kind%eof], .true.)
end subroutine dot_keypath

subroutine dot_invalid(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, ".", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine dot_invalid

subroutine comma(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, ",", &
      & [token_kind%comma, token_kind%eof], .false.)
end subroutine comma

subroutine equal(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "=", &
      & [token_kind%equal, token_kind%eof], .false.)
end subroutine equal

subroutine keypath(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "abcd", &
      & [token_kind%keypath, token_kind%eof], .true.)
end subroutine keypath

subroutine keypath_dotted(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "3.14", &
      & [token_kind%keypath, token_kind%dot, token_kind%keypath, token_kind%eof], .true.)
end subroutine keypath_dotted

subroutine keypath_whitespace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "a . b . c", &
      & [token_kind%keypath, token_kind%whitespace, token_kind%dot, token_kind%whitespace, &
      &  token_kind%keypath, token_kind%whitespace, token_kind%dot, token_kind%whitespace, &
      &  token_kind%keypath, token_kind%eof], .true.)
end subroutine keypath_whitespace

subroutine keypath_string(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "a.'b'.""c""", &
      & [token_kind%keypath, token_kind%dot, token_kind%literal, token_kind%dot, &
      &  token_kind%string, token_kind%eof], .true.)
end subroutine keypath_string

subroutine keypath_inline_table(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "a.b={c.d=1,1.2e3=""abc"",""abc""=1.2e3}", &
      & [token_kind%keypath, token_kind%dot, token_kind%keypath, token_kind%equal, &
      &  token_kind%lbrace, token_kind%keypath, token_kind%dot, token_kind%keypath, &
      &  token_kind%equal, token_kind%int, token_kind%comma, token_kind%keypath, &
      &  token_kind%dot, token_kind%keypath, token_kind%equal, token_kind%string, &
      &  token_kind%comma, token_kind%string, token_kind%equal, token_kind%float, &
      &  token_kind%rbrace, token_kind%eof], .true.)
end subroutine keypath_inline_table

subroutine keypath_inline_array(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "a.c=[""abc"",1.2e3,1,{d.f=2}]", &
      & [token_kind%keypath, token_kind%dot, token_kind%keypath, token_kind%equal, &
      &  token_kind%lbracket, token_kind%string, token_kind%comma, token_kind%float, &
      &  token_kind%comma, token_kind%int, token_kind%comma, token_kind%lbrace, &
      &  token_kind%keypath, token_kind%dot, token_kind%keypath, token_kind%equal, &
      &  token_kind%int, token_kind%rbrace, token_kind%rbracket, token_kind%eof], .true.)
end subroutine keypath_inline_array

subroutine keypath_inline_array_nested(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "data=[[""gamma"",""delta""],[1,2]]", &
      & [token_kind%keypath, token_kind%equal, token_kind%lbracket, token_kind%lbracket, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%rbracket, &
      &  token_kind%comma, token_kind%lbracket, token_kind%int, token_kind%comma, &
      &  token_kind%int, token_kind%rbracket, token_kind%rbracket, token_kind%eof], .true.)
end subroutine keypath_inline_array_nested

subroutine keypath_inline_unclosed(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "a.c=[""abc"",1.2e3,1,{d.f=2]", &
      & [token_kind%keypath, token_kind%dot, token_kind%keypath, token_kind%equal, &
      &  token_kind%unclosed, token_kind%string, token_kind%comma, token_kind%float, &
      &  token_kind%comma, token_kind%int, token_kind%comma, token_kind%unclosed, &
      &  token_kind%keypath, token_kind%dot, token_kind%keypath, token_kind%equal, &
      &  token_kind%int, token_kind%rbracket, token_kind%eof], .true.)
end subroutine keypath_inline_unclosed

subroutine keypath_invalid(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "a:b", &
      & [token_kind%invalid, token_kind%eof], .true.)
end subroutine keypath_invalid

subroutine keypath_escape(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "\u00c0", &
      & [token_kind%invalid, token_kind%eof], .true.)
end subroutine keypath_escape

subroutine literal(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "'something\','anything'", &
      & [token_kind%literal, token_kind%comma, token_kind%literal, token_kind%eof], .false.)
end subroutine literal

subroutine literal_unclosed(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "'right open"//new_line('a')//"'fully closed'", &
      & [token_kind%invalid, token_kind%newline, token_kind%literal, token_kind%eof], .false.)
end subroutine literal_unclosed

subroutine literal_control(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "'control char "//achar(0)//"','normal literal'", &
      & [token_kind%invalid, token_kind%comma, token_kind%literal, token_kind%eof], .false.)
end subroutine literal_control

subroutine literal_triple(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "'''something''','''anything'''", &
      & [token_kind%mliteral, token_kind%comma, token_kind%mliteral, token_kind%eof], .false.)
end subroutine literal_triple

subroutine literal_multiline(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "'''something"//new_line('a')//"anything'''", &
      & [token_kind%mliteral, token_kind%eof], .false.)
end subroutine literal_multiline

subroutine literal_multiline_five(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "'''something,anything'''''", &
      & [token_kind%mliteral, token_kind%eof], .false.)
end subroutine literal_multiline_five

subroutine literal_multiline_six(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "'''something,anything''''''", &
      & [token_kind%mliteral, token_kind%invalid, token_kind%eof], .false.)
end subroutine literal_multiline_six

subroutine literal_multiline_unclosed(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "'''something,anything''"//new_line('a')//"a.key = 'value'", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine literal_multiline_unclosed

subroutine string(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """something"",""anything""", &
      & [token_kind%string, token_kind%comma, token_kind%string, token_kind%eof], .false.)
end subroutine string

subroutine string_unclosed(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """right open"//new_line('a')//"""fully closed""", &
      & [token_kind%invalid, token_kind%newline, token_kind%string, token_kind%eof], .false.)
end subroutine string_unclosed

subroutine string_control(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """control char "//achar(0)//""",""normal string""", &
      & [token_kind%invalid, token_kind%comma, token_kind%string, token_kind%eof], .false.)
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
      & [token_kind%string, token_kind%comma, token_kind%string, token_kind%newline, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%newline, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%newline, &
      &  token_kind%string, token_kind%newline, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%newline, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%newline, &
      &  token_kind%string, token_kind%comma, token_kind%string, token_kind%eof], .false.)
end subroutine string_escape

subroutine string_escape_invalid(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: nl = new_line('a')

   call check_token(error, """""""something\ """""",""""""anything\u007""""""", &
      & [token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%eof], .false.)
end subroutine string_escape_invalid

subroutine string_unicode_escape(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: nl = new_line('a')

   call check_token(error, """\uD800"",""\ufffe""", &
      & [token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%eof], .false.)
end subroutine string_unicode_escape

subroutine string_triple(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """""""something"""""",""""""anything""""""", &
      & [token_kind%mstring, token_kind%comma, token_kind%mstring, token_kind%eof], .false.)
end subroutine string_triple

subroutine string_multiline(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """""""something"//new_line('a')//"anything""""""", &
      & [token_kind%mstring, token_kind%eof], .false.)
end subroutine string_multiline

subroutine string_multiline_unclosed1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """""""", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine string_multiline_unclosed1

subroutine string_multiline_unclosed2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """""""\", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine string_multiline_unclosed2

subroutine string_multiline_unclosed3(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """""""\""""""", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine string_multiline_unclosed3

subroutine string_multiline_escape(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, """""""\"//achar(10)//""""""","//&
      & """""""\"//achar(13)//achar(10)//""""""","//&
      & """""""value\n""""""", &
      & [token_kind%mstring, token_kind%comma, token_kind%mstring, token_kind%comma, &
      &  token_kind%mstring, token_kind%eof], .false.)
end subroutine string_multiline_escape

subroutine whitespace_blank(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "    ", &
      & [token_kind%whitespace, token_kind%eof], .false.)
end subroutine whitespace_blank

subroutine whitespace_tab(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, achar(9)//achar(9), &
      & [token_kind%whitespace, token_kind%eof], .false.)
end subroutine whitespace_tab

subroutine whitespace_mixed(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, achar(9)//" "//achar(9), &
      & [token_kind%whitespace, token_kind%eof], .false.)
end subroutine whitespace_mixed

subroutine newline_lf(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, achar(13)//achar(10), &
      & [token_kind%newline, token_kind%eof], .false.)
end subroutine newline_lf

subroutine newline_crlf(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, achar(10), &
      & [token_kind%newline, token_kind%eof], .false.)
end subroutine newline_crlf

subroutine newline_cr(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, achar(13), &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine newline_cr

subroutine bool_false(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "false", &
      & [token_kind%bool, token_kind%eof], .false.)
end subroutine bool_false

subroutine bool_falsey(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "falsey", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine bool_falsey

subroutine bool_true(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "true", &
      & [token_kind%bool, token_kind%eof], .false.)
end subroutine bool_true

subroutine bool_nottrue(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "true-or-not", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine bool_nottrue

subroutine float_nan(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "nan,-nan,+nan", &
      & [token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%eof], .false.)
end subroutine float_nan

subroutine float_inf(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "inf,-inf,+inf", &
      & [token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%eof], .false.)
end subroutine float_inf

subroutine float_point(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "3.14,+3.14,-3.14,0.123", &
      & [token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%eof], .false.)
end subroutine float_point

subroutine float_exponent(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "3e2,3E2,3e-2,3E+2,3e0,3.1e2,3.1E2,-1E-1", &
      & [token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%eof], .false.)
end subroutine float_exponent

subroutine float_underscore(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "3_141.5927,3141.592_7,3e1_4", &
      & [token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%eof], .false.)
end subroutine float_underscore

subroutine float_zero(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "0.0,+0.0,-0.0,0e0,0e00,+0e0,-0e0", &
      & [token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%comma, token_kind%float, token_kind%comma, &
      &  token_kind%float, token_kind%eof], .false.)
end subroutine float_zero

subroutine float_double_point(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "0..1,0.1.2", &
      & [token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%eof], .false.)
end subroutine float_double_point

subroutine float_double_expo(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1ee2,1e2e3", &
      & [token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%eof], .false.)
end subroutine float_double_expo

subroutine float_dunderscored(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1e2__3", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine float_dunderscored

subroutine float_invalid(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1.2_,1.,+1.,-1.,03.14,+03.14,-03.14", &
      & [token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%invalid, token_kind%eof], .false.)
end subroutine float_invalid

subroutine float_leading_underscore(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "_1e2,1e_2,1._2", &
      & [token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%invalid, token_kind%eof], .false.)
end subroutine float_leading_underscore

subroutine float_trailing_underscore(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1e2_,1_e2,1_.2", &
      & [token_kind%invalid, token_kind%comma, token_kind%invalid, token_kind%comma, &
      &  token_kind%invalid, token_kind%eof], .false.)
end subroutine float_trailing_underscore

subroutine integer_limits(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "9223372036854775807,-9223372036854775808", &
      & [token_kind%int, token_kind%comma, token_kind%int, token_kind%eof], .false.)
end subroutine integer_limits

subroutine integer_zero(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "0,+0,-0,0x0,0x00,0x00000,0o0,0o00,0o00000,0b0,0b00,0b00000", &
      & [token_kind%int, token_kind%comma, token_kind%int, token_kind%comma, &
      &  token_kind%int, token_kind%comma, token_kind%int, token_kind%comma, &
      &  token_kind%int, token_kind%comma, token_kind%int, token_kind%comma, &
      &  token_kind%int, token_kind%comma, token_kind%int, token_kind%comma, &
      &  token_kind%int, token_kind%comma, token_kind%int, token_kind%comma, &
      &  token_kind%int, token_kind%comma, token_kind%int, token_kind%eof], .false.)
end subroutine integer_zero

subroutine integer_hexadecimal(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "0xFa1afe1,0x00987,0xcafebabe", &
      & [token_kind%int, token_kind%comma, token_kind%int, token_kind%comma, token_kind%int, &
      &  token_kind%eof], .false.)
end subroutine integer_hexadecimal

subroutine integer_octal(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "0o766,0o01234567,0o7_6_5", &
      & [token_kind%int, token_kind%comma, token_kind%int, token_kind%comma, token_kind%int, &
      &  token_kind%eof], .false.)
end subroutine integer_octal

subroutine integer_binary(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "0b1010,0b1_0_1,0b11010110", &
      & [token_kind%int, token_kind%comma, token_kind%int, token_kind%comma, token_kind%int, &
      &  token_kind%eof], .false.)
end subroutine integer_binary

subroutine integer_leading_zero(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "001", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine integer_leading_zero

subroutine integer_underscored(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1_000,1_1_1_1", &
      & [token_kind%int, token_kind%comma, token_kind%int, token_kind%eof], .false.)
end subroutine integer_underscored

subroutine integer_dunderscored(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1__000", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine integer_dunderscored

subroutine integer_trailing_underscore(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1000_", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine integer_trailing_underscore

subroutine datetime(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1987-07-05 17:45:00Z,1987-07-05t17:45:00z", &
      & [token_kind%datetime, token_kind%comma, token_kind%datetime, token_kind%eof], .false.)
end subroutine datetime

subroutine datetime_comment(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1979-05-27 # Comment", &
      & [token_kind%datetime, token_kind%whitespace, token_kind%comment, token_kind%eof], &
      & .false.)
end subroutine datetime_comment

subroutine datetime_local_date(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1987-07-05", &
      & [token_kind%datetime, token_kind%eof], .false.)
end subroutine datetime_local_date

subroutine datetime_local_time(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "17:45:00,10:32:00.555", &
      & [token_kind%datetime, token_kind%comma, token_kind%datetime, token_kind%eof], .false.)
end subroutine datetime_local_time

subroutine datetime_local(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1987-07-05T17:45:00,1977-12-21T10:32:00.555,1987-07-05 17:45:00", &
      & [token_kind%datetime, token_kind%comma, token_kind%datetime, token_kind%comma, &
      &  token_kind%datetime, token_kind%eof], .false.)
end subroutine datetime_local

subroutine datetime_milliseconds(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1987-07-05T17:45:56.1234Z,1987-07-05T17:45:56.6Z"//new_line('a')//&
      & "1987-07-05T17:45:56.1234+08:00,1987-07-05T17:45:56.6+08:00", &
      & [token_kind%datetime, token_kind%comma, token_kind%datetime, token_kind%newline, &
      &  token_kind%datetime, token_kind%comma, token_kind%datetime, token_kind%eof], .false.)
end subroutine datetime_milliseconds

subroutine datetime_timezone(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1987-07-05T17:45:56Z,1987-07-05T17:45:56-05:00"//new_line('a')//&
      & "1987-07-05T17:45:56+12:00,1987-07-05T17:45:56+13:00", &
      & [token_kind%datetime, token_kind%comma, token_kind%datetime, token_kind%newline, &
      &  token_kind%datetime, token_kind%comma, token_kind%datetime, token_kind%eof], .false.)
end subroutine datetime_timezone

subroutine datetime_hour_over(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "2006-01-01T24:00:00-00:00", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_hour_over

subroutine datetime_mday_over(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "2006-01-32T00:00:00-00:00", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_mday_over

subroutine datetime_mday_under(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "2006-01-00T00:00:00-00:00", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_mday_under

subroutine datetime_mday_leap(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "2000-02-29T00:00:00-00:00", &
      & [token_kind%datetime, token_kind%eof], .false.)
end subroutine datetime_mday_leap

subroutine datetime_minute_over(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "2006-01-01T00:60:00-00:00", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_minute_over

subroutine datetime_month_over(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "2006-13-01T00:00:00-00:00", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_month_over

subroutine datetime_month_under(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "2006-00-01T00:00:00-00:00", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_month_under

subroutine datetime_no_leading_zeros(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1987-07-5T17:45:00.12Z", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_no_leading_zeros

subroutine datetime_no_seconds(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1987-07-05T17:45Z", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_no_seconds

subroutine datetime_no_separator(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1987-07-0517:45:00Z", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_no_separator

subroutine datetime_trailing_separator(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "1987-07-05T", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_trailing_separator

subroutine datetime_local_timezone(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_token(error, "00:00:00-00:00", &
      & [token_kind%invalid, token_kind%eof], .false.)
end subroutine datetime_local_timezone

subroutine token_keypath_string(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   character(:), allocatable :: str1, str2

   call new_lexer_from_string(lexer, """keypath""")
   call lexer%extract(toml_token(token_kind%keypath, 2, 8), str1)
   call lexer%extract(toml_token(token_kind%string, 1, 9), str2)
   call check(error, str1, str2)
end subroutine token_keypath_string

subroutine token_keypath_mstring(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   character(:), allocatable :: str1, str2

   call new_lexer_from_string(lexer, """""""keypath""""""")
   call lexer%extract(toml_token(token_kind%keypath, 4, 10), str1)
   call lexer%extract(toml_token(token_kind%mstring, 1, 13), str2)
   call check(error, str1, str2)
end subroutine token_keypath_mstring

subroutine token_keypath_literal(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   character(:), allocatable :: str1, str2

   call new_lexer_from_string(lexer, "'keypath'")
   call lexer%extract(toml_token(token_kind%keypath, 2, 8), str1)
   call lexer%extract(toml_token(token_kind%literal, 1, 9), str2)
   call check(error, str1, str2)
end subroutine token_keypath_literal

subroutine token_keypath_mliteral(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   character(:), allocatable :: str1, str2

   call new_lexer_from_string(lexer, "'''keypath'''")
   call lexer%extract(toml_token(token_kind%keypath, 4, 10), str1)
   call lexer%extract(toml_token(token_kind%mliteral, 1, 13), str2)
   call check(error, str1, str2)
end subroutine token_keypath_mliteral

subroutine token_integer(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   integer(tfi) :: val1

   call new_lexer_from_string(lexer, "1023,2_047,-612")
   call lexer%extract(toml_token(token_kind%int, 1, 4), val1)
   call check(error, val1, 1023_tfi)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%int, 6, 10), val1)
   call check(error, val1, 2047_tfi)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%int, 12, 16), val1)
   call check(error, val1, -612_tfi)
   if (allocated(error)) return
end subroutine token_integer

subroutine token_integer_binary(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   integer(tfi) :: val1

   call new_lexer_from_string(lexer, "0b1001,0b0_100,-0b010")
   call lexer%extract(toml_token(token_kind%int, 1, 6), val1)
   call check(error, val1, int(b"1001", tfi))
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%int, 8, 14), val1)
   call check(error, val1, int(b"0100", tfi))
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%int, 16, 22), val1)
   call check(error, val1, -int(b"010", tfi))
   if (allocated(error)) return
end subroutine token_integer_binary

subroutine token_integer_octal(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   integer(tfi) :: val1

   call new_lexer_from_string(lexer, "0o1023,0o2_047,-0o612")
   call lexer%extract(toml_token(token_kind%int, 1, 6), val1)
   call check(error, val1, int(o"1023", tfi))
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%int, 8, 14), val1)
   call check(error, val1, int(o"2047", tfi))
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%int, 16, 22), val1)
   call check(error, val1, -int(o"612", tfi))
   if (allocated(error)) return
end subroutine token_integer_octal

subroutine token_integer_hexadecimal(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   integer(tfi) :: val1

   call new_lexer_from_string(lexer, "0x1aB1,0x0_19f,-0xF1c")
   call lexer%extract(toml_token(token_kind%int, 1, 6), val1)
   call check(error, val1, int(z"1aB1", tfi))
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%int, 8, 14), val1)
   call check(error, val1, int(z"019f", tfi))
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%int, 16, 22), val1)
   call check(error, val1, -int(z"F1c", tfi))
end subroutine token_integer_hexadecimal

subroutine token_float(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   real(tfr) :: val1

   call new_lexer_from_string(lexer, "1023.0,2_000.47,-6.12e-1,1e0,1e+0,1e-0")
   call lexer%extract(toml_token(token_kind%float, 1, 6), val1)
   call check(error, val1, 1023.0_tfr)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 8, 15), val1)
   call check(error, val1, 2000.47_tfr)
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

subroutine token_float_exceptional(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   real(tfr) :: val1

   call new_lexer_from_string(lexer, "nan,+nan,-nan,inf,+inf,-inf")
   call lexer%extract(toml_token(token_kind%float, 1, 3), val1)
   call check(error, val1 /= val1)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 5, 8), val1)
   call check(error, val1 /= val1)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 10, 13), val1)
   call check(error, val1 /= val1)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 15, 17), val1)
   call check(error, val1 > huge(val1))
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 19, 22), val1)
   call check(error, val1 > huge(val1))
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%float, 24, 27), val1)
   call check(error, val1 < -huge(val1))
   if (allocated(error)) return
end subroutine token_float_exceptional

subroutine token_float_fuzz(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: it
   type(toml_lexer) :: lexer
   real(tfr) :: val1, val2
   character(128) :: buffer

   do it = 1, 200
      call random_number(val1)
      call random_number(val2)
      val1 = (val1 - 0.5_tfr) ** nint(it * abs(val2))
      write(buffer, *) val1

      call new_lexer_from_string(lexer, buffer)
      call lexer%extract(toml_token(token_kind%float, 1, len_trim(buffer)), val2)
      call check(error, val2, val1, thr=2*epsilon(val1))
      if (allocated(error)) exit
   end do
end subroutine token_float_fuzz

subroutine token_datetime(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   type(toml_token) :: token
   type(toml_datetime) :: val, ref
   character(*), parameter :: str = "1987-07-05 17:45:00z"

   call new_lexer_from_string(lexer, str)

   token = toml_token(token_kind%datetime, 1, 10)
   ref = toml_datetime(year=1987, month=7, day=5)
   call lexer%extract(token, val)
   call check(error, val == ref, &
      & "Extraction of '"//str(token%first:token%last)//"' failed, "//&
      & "expected '"//to_string(ref)//"', got '"//to_string(val)//"'")
   if (allocated(error)) return

   token = toml_token(token_kind%datetime, 1, 19)
   call lexer%extract(token, val)
   ref = toml_datetime(year=1987, month=7, day=5, hour=17, minute=45, second=0)
   call check(error, val == ref, &
      & "Extraction of '"//str(token%first:token%last)//"' failed")
   if (allocated(error)) return

   token = toml_token(token_kind%datetime, 12, 19)
   call lexer%extract(token, val)
   ref = toml_datetime(hour=17, minute=45, second=0)
   call check(error, val == ref, &
      & "Extraction of '"//str(token%first:token%last)//"' failed")
   if (allocated(error)) return

   token = toml_token(token_kind%datetime, 1, 20)
   call lexer%extract(token, val)
   ref = toml_datetime(year=1987, month=7, day=5, hour=17, minute=45, second=0, &
      & zone="Z")
   call check(error, val == ref, &
      & "Extraction of '"//str(token%first:token%last)//"' failed")
   if (allocated(error)) return
end subroutine token_datetime

subroutine token_bool(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   logical :: val

   call new_lexer_from_string(lexer, "true,false")
   call lexer%extract(toml_token(token_kind%bool, 1, 4), val)
   call check(error, val .eqv. .true.)
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%bool, 6, 10), val)
   call check(error, val .eqv. .false.)
   if (allocated(error)) return
end subroutine token_bool

subroutine token_string(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   character(:), allocatable :: val

   call new_lexer_from_string(lexer, """\u03B4"",""\U000003B4""")
   call lexer%extract(toml_token(token_kind%string, 1, 8), val)
   call check(error, val, "δ")
   if (allocated(error)) return
   call lexer%extract(toml_token(token_kind%string, 10, 21), val)
   call check(error, val, "δ")
   if (allocated(error)) return
end subroutine token_string

subroutine lexer_from_sequential(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(toml_lexer) :: lexer
   type(toml_token) :: token
   type(toml_error), allocatable :: parse_error
   character(:), allocatable :: filename
   integer :: io

   filename = get_name()
   open(file=filename, newunit=io)
   write(io, "(a)") "abc"
   close(io)

   open(file=filename, newunit=io)
   call new_lexer_from_unit(lexer, io, parse_error)
   call move_error(error, parse_error)
   close(io, status="delete")
   if (allocated(error)) return

   call lexer%next(token)
   call check(error, token%kind, token_kind%keypath)
end subroutine lexer_from_sequential

subroutine check_token(error, string, expected, keypath)
   use tomlf_diagnostic, only : render, toml_label, toml_level
   use tomlf_terminal, only : toml_terminal
   !> Error handling
   type(error_type), allocatable, intent(out) :: error
   !> String to be parsed
   character(len=*), intent(in) :: string
   !> Expected token kind
   integer, intent(in) :: expected(:)
   !> Part of a keypath
   logical, intent(in) :: keypath

   integer :: it
   logical :: okay
   type(toml_lexer) :: lexer
   type(toml_token) :: token
   type(toml_label), allocatable :: label(:)

   call new_lexer_from_string(lexer, string)
   lexer%top = 1
   lexer%stack(1)%scope = merge(1, 3, keypath)

   allocate(label(0))
   do it = 1, size(expected)
      call lexer%next(token)
      okay = token%kind == expected(it)
      ! label = [label, toml_label(merge(level_info, level_error, okay), &
      !    &     token%first, token%last, stringify(token), .not.okay)]
      ! msg = render(string//new_line('a'), [label(size(label))], toml_terminal(.true.))
      call check(error, token%kind, expected(it), &
         & "Expected '"//stringify(toml_token(expected(it)))// &
         & "' but got '"//stringify(token)//"'")!//new_line('a')//msg)
      if (allocated(error)) exit
   end do
   ! msg = render(string//new_line('a'), label, toml_terminal(.true.))
   ! print '(a)', msg
end subroutine check_token

subroutine move_error(error, parse_error)
   type(error_type), allocatable, intent(out) :: error
   type(toml_error), intent(in), optional :: parse_error

   if (present(parse_error)) then
      allocate(error)
      error%stat = 1
      error%message = parse_error%message
   end if
end subroutine move_error

function get_name() result(filename)
   character(len=15) :: filename

   real :: val

   call random_number(val)
   write(filename, '(a, z8.8)') "toml-f-", int(val*1.0e9)
end function

end module tftest_lexer
