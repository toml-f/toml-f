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

module tftest_parser
   use testdrive
   use tomlf_constants, only : nl => TOML_NEWLINE
   use tomlf_de_parser
   use tomlf_de_lexer, only : toml_lexer, new_lexer_from_string, toml_token, token_kind
   use tomlf_error, only : toml_error
   use tomlf_type, only : toml_table
   use tomlf_terminal, only : toml_terminal
   implicit none

   public :: collect_parser

   type, extends(toml_lexer) :: mocked_lexer
      type(toml_token), allocatable :: token(:)
   contains
      procedure :: next
   end type mocked_lexer

contains

!> Collect all exported unit tests
subroutine collect_parser(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("root-invalid", root_invalid, should_fail=.true.), &
      & new_unittest("table-body", table_body), &
      & new_unittest("table-header-long", table_header_long), &
      & new_unittest("table-header-empty", table_header_empty, should_fail=.true.), &
      & new_unittest("table-header-unclosed1", table_header_unclosed1, should_fail=.true.), &
      & new_unittest("table-header-unclosed2", table_header_unclosed2, should_fail=.true.), &
      & new_unittest("table-header-invalid", table_header_invalid, should_fail=.true.), &
      & new_unittest("table-header-newline1", table_header_newline1, should_fail=.true.), &
      & new_unittest("table-header-newline2", table_header_newline2, should_fail=.true.), &
      & new_unittest("table-header-duplicate", table_header_duplicate, should_fail=.true.), &
      & new_unittest("table-header-trailing-whitespace", table_header_trailing_whitespace), &
      & new_unittest("table-header-trailing-comment", table_header_trailing_comment), &
      & new_unittest("aot-body", aot_body), &
      & new_unittest("aot-inline", aot_inline, should_fail=.true.), &
      & new_unittest("aot-header-whitespace1", aot_header_whitespace1), &
      & new_unittest("aot-header-whitespace2", aot_header_whitespace2), &
      & new_unittest("aot-header-empty", aot_header_empty, should_fail=.true.), &
      & new_unittest("aot-header-unclosed1", aot_header_unclosed1, should_fail=.true.), &
      & new_unittest("aot-header-unclosed2", aot_header_unclosed2, should_fail=.true.), &
      & new_unittest("aot-header-unclosed3", aot_header_unclosed3, should_fail=.true.), &
      & new_unittest("aot-header-duplicate1", aot_header_duplicate1, should_fail=.true.), &
      & new_unittest("aot-header-duplicate2", aot_header_duplicate2, should_fail=.true.), &
      & new_unittest("aot-header-duplicate3", aot_header_duplicate3, should_fail=.true.), &
      & new_unittest("aot-header-invalid1", aot_header_invalid1, should_fail=.true.), &
      & new_unittest("aot-header-invalid2", aot_header_invalid2, should_fail=.true.), &
      & new_unittest("keyval-empty", keyval_empty, should_fail=.true.), &
      & new_unittest("keyval-equal", keyval_equal, should_fail=.true.), &
      & new_unittest("keyval-dotted", keyval_dotted), &
      & new_unittest("keyval-comment", keyval_comment), &
      & new_unittest("inline-array", inline_array), &
      & new_unittest("inline-array-empty", inline_array_empty), &
      & new_unittest("inline-array-whitespace", inline_array_whitespace), &
      & new_unittest("inline-array-newline", inline_array_newline), &
      & new_unittest("inline-array-nested", inline_array_nested), &
      & new_unittest("inline-table", inline_table), &
      & new_unittest("inline-table-empty", inline_table_empty), &
      & new_unittest("inline-table-whitespace", inline_table_whitespace), &
      & new_unittest("inline-table-newline", inline_table_newline, should_fail=.true.), &
      & new_unittest("inline-table-comma", inline_table_comma, should_fail=.true.), &
      & new_unittest("inline-table-modify", inline_table_modify, should_fail=.true.), &
      & new_unittest("empty", empty)]

end subroutine collect_parser

subroutine empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "", [toml_token(token_kind%eof)])
end subroutine empty

subroutine root_invalid(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "}", &
      & [toml_token(token_kind%lbrace, 1, 1), toml_token(token_kind%eof, 2, 2)])
end subroutine root_invalid

subroutine table_body(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a]"//nl//"c=1"//nl//"[b]"//nl//"c=1", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%rbracket, 3, 3), toml_token(token_kind%newline, 4, 4), &
      &  toml_token(token_kind%keypath, 5, 5), toml_token(token_kind%equal, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%newline, 8, 8), &
      &  toml_token(token_kind%lbracket, 9, 9), toml_token(token_kind%keypath, 10, 10), &
      &  toml_token(token_kind%rbracket, 11, 11), toml_token(token_kind%newline, 12, 12), &
      &  toml_token(token_kind%keypath, 13, 13), toml_token(token_kind%equal, 14, 14), &
      &  toml_token(token_kind%int, 15, 15), toml_token(token_kind%eof, 16, 16)])
end subroutine table_body

subroutine aot_body(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[a]]"//nl//"b=1"//nl//"[[a]]"//nl//"b=1", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%newline, 6, 6), &
      &  toml_token(token_kind%keypath, 7, 7), toml_token(token_kind%equal, 8, 8), &
      &  toml_token(token_kind%int, 9, 9), toml_token(token_kind%newline, 10, 10), &
      &  toml_token(token_kind%lbracket, 11, 11), toml_token(token_kind%lbracket, 12, 12), &
      &  toml_token(token_kind%keypath, 13, 13), toml_token(token_kind%rbracket, 14, 14), &
      &  toml_token(token_kind%rbracket, 15, 15), toml_token(token_kind%newline, 16, 16), &
      &  toml_token(token_kind%keypath, 17, 17), toml_token(token_kind%equal, 18, 18), &
      &  toml_token(token_kind%int, 19, 19), toml_token(token_kind%eof, 20, 20)])
end subroutine aot_body

subroutine table_header_long(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%dot, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%dot, 5, 5), toml_token(token_kind%keypath, 6, 6), &
      &  toml_token(token_kind%dot, 7, 7), toml_token(token_kind%keypath, 8, 8), &
      &  toml_token(token_kind%dot, 9, 9), toml_token(token_kind%keypath, 10, 10), &
      &  toml_token(token_kind%dot, 11, 11), toml_token(token_kind%keypath, 12, 12), &
      &  toml_token(token_kind%dot, 13, 13), toml_token(token_kind%keypath, 14, 14), &
      &  toml_token(token_kind%dot, 15, 15), toml_token(token_kind%keypath, 16, 16), &
      &  toml_token(token_kind%dot, 17, 17), toml_token(token_kind%keypath, 18, 18), &
      &  toml_token(token_kind%dot, 19, 19), toml_token(token_kind%keypath, 20, 20), &
      &  toml_token(token_kind%dot, 21, 21), toml_token(token_kind%keypath, 22, 22), &
      &  toml_token(token_kind%dot, 23, 23), toml_token(token_kind%keypath, 24, 24), &
      &  toml_token(token_kind%dot, 25, 25), toml_token(token_kind%keypath, 26, 26), &
      &  toml_token(token_kind%dot, 27, 27), toml_token(token_kind%keypath, 28, 28), &
      &  toml_token(token_kind%dot, 29, 29), toml_token(token_kind%keypath, 30, 30), &
      &  toml_token(token_kind%dot, 31, 31), toml_token(token_kind%keypath, 32, 32), &
      &  toml_token(token_kind%dot, 33, 33), toml_token(token_kind%keypath, 34, 34), &
      &  toml_token(token_kind%dot, 35, 35), toml_token(token_kind%keypath, 36, 36), &
      &  toml_token(token_kind%dot, 37, 37), toml_token(token_kind%keypath, 38, 38), &
      &  toml_token(token_kind%dot, 39, 39), toml_token(token_kind%keypath, 40, 40), &
      &  toml_token(token_kind%dot, 41, 41), toml_token(token_kind%keypath, 42, 42), &
      &  toml_token(token_kind%dot, 43, 43), toml_token(token_kind%keypath, 44, 44), &
      &  toml_token(token_kind%dot, 45, 45), toml_token(token_kind%keypath, 46, 46), &
      &  toml_token(token_kind%dot, 47, 47), toml_token(token_kind%keypath, 48, 48), &
      &  toml_token(token_kind%dot, 49, 49), toml_token(token_kind%keypath, 50, 50), &
      &  toml_token(token_kind%dot, 51, 51), toml_token(token_kind%keypath, 52, 52), &
      &  toml_token(token_kind%rbracket, 53, 53), toml_token(token_kind%eof, 54, 54)])
end subroutine table_header_long

subroutine table_header_empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[]", &
      & [toml_token(token_kind%lbracket, 60, 1), toml_token(token_kind%rbracket, 2, 2), &
      &  toml_token(token_kind%eof, 15, 9)])
end subroutine table_header_empty

subroutine table_header_unclosed1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%eof, 2, 2)])
end subroutine table_header_unclosed1

subroutine table_header_unclosed2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a.]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%dot, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%eof, 5, 5)])
end subroutine table_header_unclosed2

subroutine table_header_invalid(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a,]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%comma, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%eof, 5, 5)])
end subroutine table_header_invalid

subroutine table_header_newline1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[""\n""]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%string, 2, 5), &
      &  toml_token(token_kind%rbracket, 6, 6), toml_token(token_kind%eof, 7, 7)])
end subroutine table_header_newline1

subroutine table_header_newline2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a] [b]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%rbracket, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%lbracket, 5, 5), toml_token(token_kind%keypath, 6, 6), &
      &  toml_token(token_kind%rbracket, 7, 7), toml_token(token_kind%eof, 8, 8)])
end subroutine table_header_newline2

subroutine table_header_duplicate(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a]"//nl//"[a]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%rbracket, 3, 3), toml_token(token_kind%newline, 4, 4), &
      &  toml_token(token_kind%lbracket, 5, 5), toml_token(token_kind%keypath, 6, 6), &
      &  toml_token(token_kind%rbracket, 7, 7), toml_token(token_kind%eof, 5, 5)])
end subroutine table_header_duplicate

subroutine table_header_trailing_whitespace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a.b] ", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%dot, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%whitespace, 6, 6), &
      &  toml_token(token_kind%eof, 7, 7)])
end subroutine table_header_trailing_whitespace

subroutine table_header_trailing_comment(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a.b]#", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%dot, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%comment, 6, 6), &
      &  toml_token(token_kind%eof, 7, 7)])
end subroutine table_header_trailing_comment

subroutine aot_header_whitespace1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[ a]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%whitespace, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%rbracket, 6, 6), &
      &  toml_token(token_kind%eof, 7, 7)])
end subroutine aot_header_whitespace1

subroutine aot_header_whitespace2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[a ]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%rbracket, 6, 6), &
      &  toml_token(token_kind%eof, 7, 7)])
end subroutine aot_header_whitespace2

subroutine aot_header_empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%rbracket, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%eof, 5, 5)])
end subroutine aot_header_empty

subroutine aot_header_unclosed1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%eof, 3, 3)])
end subroutine aot_header_unclosed1

subroutine aot_header_unclosed2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[a.]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%dot, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%rbracket, 6, 6), &
      &  toml_token(token_kind%eof, 7, 7)])
end subroutine aot_header_unclosed2

subroutine aot_header_unclosed3(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[ [a]  #", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%whitespace, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%whitespace, 6, 7), &
      &  toml_token(token_kind%comment, 8, 8), toml_token(token_kind%eof, 9, 9)])
end subroutine aot_header_unclosed3

subroutine aot_header_duplicate1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[a]]"//nl//"[a]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%newline, 6, 6), &
      &  toml_token(token_kind%lbracket, 7, 7), toml_token(token_kind%keypath, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine aot_header_duplicate1

subroutine aot_header_duplicate2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a]"//nl//"[[a]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%rbracket, 3, 3), toml_token(token_kind%newline, 4, 4), &
      &  toml_token(token_kind%lbracket, 5, 5), toml_token(token_kind%lbracket, 6, 6), &
      &  toml_token(token_kind%keypath, 7, 7), toml_token(token_kind%rbracket, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine aot_header_duplicate2

subroutine aot_header_duplicate3(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[a.b.c]"//nl//"[[a.b]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%keypath, 2, 2), &
      &  toml_token(token_kind%dot, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%dot, 5, 5), toml_token(token_kind%keypath, 6, 6), &
      &  toml_token(token_kind%rbracket, 7, 7), toml_token(token_kind%newline, 8, 8), &
      &  toml_token(token_kind%lbracket, 9, 9), toml_token(token_kind%lbracket, 10, 10), &
      &  toml_token(token_kind%keypath, 11, 11), toml_token(token_kind%dot, 12, 12), &
      &  toml_token(token_kind%keypath, 13, 13), toml_token(token_kind%rbracket, 14, 14), &
      &  toml_token(token_kind%rbracket, 15, 15), toml_token(token_kind%eof, 16, 16)])
end subroutine aot_header_duplicate3

subroutine aot_header_invalid1(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[ [a]]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%whitespace, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%rbracket, 5, 5), toml_token(token_kind%rbracket, 6, 6), &
      &  toml_token(token_kind%eof, 6, 6)])
end subroutine aot_header_invalid1

subroutine aot_header_invalid2(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "[[a] ]", &
      & [toml_token(token_kind%lbracket, 1, 1), toml_token(token_kind%lbracket, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%whitespace, 5, 5), toml_token(token_kind%rbracket, 6, 6), &
      &  toml_token(token_kind%eof, 6, 6)])
end subroutine aot_header_invalid2

subroutine aot_inline(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[]"//nl//"[[a]]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%newline, 6, 6), toml_token(token_kind%lbracket, 6, 6), &
      &  toml_token(token_kind%lbracket, 7, 7), toml_token(token_kind%keypath, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%rbracket, 10, 10), &
      &  toml_token(token_kind%eof, 6, 6)])
end subroutine aot_inline

subroutine keyval_empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%eof, 3, 3)])
end subroutine keyval_empty

subroutine keyval_equal(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=,", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%comma, 3, 3), toml_token(token_kind%eof, 4, 4)])
end subroutine keyval_equal

subroutine keyval_dotted(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a.b.c=1", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%dot, 2, 2), &
      &  toml_token(token_kind%keypath, 3, 3), toml_token(token_kind%dot, 4, 4), &
      &  toml_token(token_kind%keypath, 5, 5), toml_token(token_kind%equal, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%eof, 8, 8)])
end subroutine keyval_dotted

subroutine keyval_comment(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "# This is a full-line comment"//nl//&
      & "key = ""value"" # This is a comment at the end of a line", &
      & [toml_token(token_kind%comment, 1, 29), toml_token(token_kind%newline, 30, 30), &
      &  toml_token(token_kind%keypath, 31, 33), toml_token(token_kind%whitespace, 34, 34), &
      &  toml_token(token_kind%equal, 35, 35), toml_token(token_kind%whitespace, 36, 36), &
      &  toml_token(token_kind%string, 37, 43), toml_token(token_kind%whitespace, 44, 44), &
      &  toml_token(token_kind%comment, 45, 85), toml_token(token_kind%eof, 86, 86)])
end subroutine keyval_comment

subroutine inline_array(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[1,[2,],[3]]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%int, 4, 4), &
      &  toml_token(token_kind%comma, 5, 5), toml_token(token_kind%lbracket, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%comma, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%comma, 10, 10), &
      &  toml_token(token_kind%lbracket, 11, 11), toml_token(token_kind%int, 12, 12), &
      &  toml_token(token_kind%rbracket, 13, 13), toml_token(token_kind%rbracket, 14, 14), &
      &  toml_token(token_kind%eof, 15, 15)])
end subroutine inline_array

subroutine inline_array_empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%rbracket, 4, 4), &
      &  toml_token(token_kind%eof, 5, 5)])
end subroutine inline_array_empty

subroutine inline_array_whitespace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[ 1 , ]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%int, 5, 5), toml_token(token_kind%whitespace, 6, 6), &
      &  toml_token(token_kind%comma, 7, 7), toml_token(token_kind%whitespace, 8, 8), &
      &  toml_token(token_kind%rbracket, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine inline_array_whitespace

subroutine inline_array_newline(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a=[ #"//nl//"1 #"//nl//", #"//nl//"] #", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbracket, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%comment, 5, 5), toml_token(token_kind%newline, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%whitespace, 8, 8), &
      &  toml_token(token_kind%comment, 9, 9), toml_token(token_kind%newline, 10, 10), &
      &  toml_token(token_kind%comma, 11, 11), toml_token(token_kind%whitespace, 12, 12), &
      &  toml_token(token_kind%comment, 13, 13), toml_token(token_kind%newline, 14, 14), &
      &  toml_token(token_kind%rbracket, 15, 15), toml_token(token_kind%whitespace, 16, 16), &
      &  toml_token(token_kind%comment, 17, 17), toml_token(token_kind%eof, 18, 18)])
end subroutine inline_array_newline


subroutine inline_array_nested(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "data=[[""gamma"",""delta""],[1,2]]", &
      & [toml_token(token_kind%keypath, 1, 4), toml_token(token_kind%equal, 5, 5), &
      &  toml_token(token_kind%lbracket, 6, 6), toml_token(token_kind%lbracket, 7, 7), &
      &  toml_token(token_kind%string, 8, 14), toml_token(token_kind%comma, 15, 15), &
      &  toml_token(token_kind%string, 16, 22), toml_token(token_kind%rbracket, 23, 23), &
      &  toml_token(token_kind%comma, 24, 24), toml_token(token_kind%lbracket, 25, 25), &
      &  toml_token(token_kind%int, 26, 26), toml_token(token_kind%comma, 27, 27), &
      &  toml_token(token_kind%int, 28, 28), toml_token(token_kind%rbracket, 29, 29), &
      &  toml_token(token_kind%rbracket, 30, 30), toml_token(token_kind%eof, 31, 31)])
end subroutine inline_array_nested

subroutine inline_table(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={b=1,c.d=2}", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%keypath, 4, 4), &
      &  toml_token(token_kind%equal, 5, 5), toml_token(token_kind%int, 6, 6), &
      &  toml_token(token_kind%comma, 7, 7), toml_token(token_kind%keypath, 8, 8), &
      &  toml_token(token_kind%dot, 9, 9), toml_token(token_kind%keypath, 10, 10), &
      &  toml_token(token_kind%equal, 11, 11), toml_token(token_kind%int, 12, 12), &
      &  toml_token(token_kind%rbrace, 13, 13), toml_token(token_kind%eof, 14, 14)])
end subroutine inline_table

subroutine inline_table_empty(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={}", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%rbrace, 4, 4), &
      &  toml_token(token_kind%eof, 5, 5)])
end subroutine inline_table_empty

subroutine inline_table_empty_whitespace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={ }", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%rbrace, 5, 5), toml_token(token_kind%eof, 6, 6)])
end subroutine inline_table_empty_whitespace

subroutine inline_table_whitespace(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={ b=1 }", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%keypath, 5, 5), toml_token(token_kind%equal, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%whitespace, 8, 8), &
      &  toml_token(token_kind%rbrace, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine inline_table_whitespace

subroutine inline_table_newline(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={ b=1"//nl//"}", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%keypath, 5, 5), toml_token(token_kind%equal, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%newline, 8, 8), &
      &  toml_token(token_kind%rbrace, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine inline_table_newline

subroutine inline_table_comma(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={ b=1,}", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%whitespace, 4, 4), &
      &  toml_token(token_kind%keypath, 5, 5), toml_token(token_kind%equal, 6, 6), &
      &  toml_token(token_kind%int, 7, 7), toml_token(token_kind%comma, 8, 8), &
      &  toml_token(token_kind%rbrace, 9, 9), toml_token(token_kind%eof, 10, 10)])
end subroutine inline_table_comma


subroutine inline_table_modify(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check_parser(error, "a={}"//nl//"[a.b]", &
      & [toml_token(token_kind%keypath, 1, 1), toml_token(token_kind%equal, 2, 2), &
      &  toml_token(token_kind%lbrace, 3, 3), toml_token(token_kind%rbrace, 4, 4), &
      &  toml_token(token_kind%newline, 5, 5), toml_token(token_kind%lbracket, 6, 6), &
      &  toml_token(token_kind%keypath, 7, 7), toml_token(token_kind%dot, 8, 8), &
      &  toml_token(token_kind%keypath, 9, 9), toml_token(token_kind%rbracket, 10, 10), &
      &  toml_token(token_kind%eof, 11, 11)])
end subroutine inline_table_modify

subroutine check_parser(error, string, token)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error
   character(len=*), intent(in) :: string
   type(toml_token), intent(in) :: token(:)

   type(mocked_lexer) :: lexer
   type(toml_error), allocatable :: parse_error
   type(toml_table), allocatable :: table

   call new_lexer(lexer, string, token)

   call parse(lexer, table, config=toml_parser_config(color=.true.), error=parse_error)
   call move_error(error, parse_error)
   if (allocated(error)) return
   call check(error, allocated(table))
end subroutine check_parser

!> Create a new lexer
subroutine new_lexer(lexer, string, token)
   type(mocked_lexer), intent(out) :: lexer
   character(len=*), intent(in) :: string
   type(toml_token), intent(in) :: token(:)

   lexer%filename = "mocked"
   lexer%pos = 0
   lexer%chunk = string
   lexer%token = token
end subroutine new_lexer

subroutine next(lexer, token)
   class(mocked_lexer), intent(inout) :: lexer
   type(toml_token), intent(inout) :: token

   lexer%pos = lexer%pos + 1
   token = lexer%token(lexer%pos)
end subroutine next

subroutine move_error(error, parse_error)
   type(error_type), allocatable, intent(out) :: error
   type(toml_error), intent(in), optional :: parse_error

   if (present(parse_error)) then
      allocate(error)
      error%stat = 1
      error%message = parse_error%message
   end if
end subroutine move_error

end module tftest_parser
