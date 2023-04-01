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

!> Implementation of a parser for transforming a token stream to TOML datastructures.
module tomlf_de_parser
   use tomlf_constants, only : tfc, tfr, tfi, TOML_NEWLINE
   use tomlf_datetime, only : toml_datetime
   use tomlf_de_context, only : toml_context
   use tomlf_de_abc, only : toml_lexer => abstract_lexer
   use tomlf_de_token, only : toml_token, token_kind, stringify
   use tomlf_diagnostic, only : render, toml_diagnostic, toml_label, toml_level
   use tomlf_terminal, only : toml_terminal
   use tomlf_error, only : toml_error, toml_stat
   use tomlf_type, only : toml_table, toml_array, toml_keyval, toml_value, toml_key, &
      & add_table, add_array, add_keyval, cast_to_table, cast_to_array, len
   implicit none
   private

   public :: toml_parser, toml_parser_config, parse


   !> Configuration of the TOML parser
   type :: toml_parser_config
      !> Use colorful output for diagnostics
      type(toml_terminal) :: color = toml_terminal()
      !> Record all tokens
      integer :: context_detail = 0
   end type toml_parser_config

   interface toml_parser_config
      module procedure :: new_parser_config
   end interface toml_parser_config

   !> TOML parser
   type :: toml_parser
      !> Current token
      type(toml_token) :: token
      !> Table containing the document root
      type(toml_table), allocatable :: root
      !> Pointer to the currently processed table
      type(toml_table), pointer :: current
      !> Diagnostic produced while parsing
      type(toml_diagnostic), allocatable :: diagnostic
      !> Context for producing diagnostics
      type(toml_context) :: context
      !> Configuration of the parser
      type(toml_parser_config) :: config
   end type toml_parser

contains

!> Create a new instance of the TOML parser
subroutine new_parser(parser, config)
   !> Instance of the parser
   type(toml_parser), intent(out), target :: parser
   !> Configuration of the parser
   type(toml_parser_config), intent(in), optional :: config

   parser%token = toml_token(token_kind%newline, 0, 0)
   parser%root = toml_table()
   parser%current => parser%root
   parser%config = toml_parser_config()
   if (present(config)) parser%config = config
end subroutine new_parser

!> Create new configuration for the TOML parser
pure function new_parser_config(color, context_detail) result(config)
   !> Configuration of the parser
   type(toml_parser_config) :: config
   !> Color support for diagnostics
   logical, intent(in), optional :: color
   !> Record all tokens
   integer, intent(in), optional :: context_detail

   if (present(color)) config%color = toml_terminal(color)
   if (present(context_detail)) config%context_detail = context_detail
end function new_parser_config

!> Parse TOML document and return root table
subroutine parse(lexer, table, config, context, error)
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> TOML data structure
   type(toml_table), allocatable, intent(out) :: table
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handler
   type(toml_error), allocatable, intent(out), optional :: error

   type(toml_parser) :: parser

   call new_parser(parser, config)
   call parse_root(parser, lexer)

   if (present(error) .and. allocated(parser%diagnostic)) then
      call make_error(error, parser%diagnostic, lexer, parser%config%color)
   end if
   if (allocated(parser%diagnostic)) return

   call move_alloc(parser%root, table)

   if (present(context)) then
      context = parser%context
      call lexer%get_info("filename", context%filename)
      call lexer%get_info("source", context%source)
   end if
end subroutine parse

!> Parse the root table
subroutine parse_root(parser, lexer)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer

   do while(.not.allocated(parser%diagnostic) .and. parser%token%kind /= token_kind%eof)
      select case(parser%token%kind)
      case(token_kind%newline, token_kind%whitespace, token_kind%comment)
         call next_token(parser, lexer)

      case(token_kind%keypath, token_kind%string, token_kind%literal)
         call parse_keyval(parser, lexer, parser%current)

      case(token_kind%lbracket)
         call parse_table_header(parser, lexer)

      case default
         call syntax_error(parser%diagnostic, lexer, parser%token, &
            & "Invalid syntax", &
            & "unexpected "//stringify(parser%token))
      end select
   end do
end subroutine parse_root


!> Parse a table or array of tables header
subroutine parse_table_header(parser, lexer)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer

   type(toml_array), pointer :: array
   type(toml_table), pointer :: table
   class(toml_value), pointer :: ptr
   type(toml_key) :: key
   logical :: array_of_tables

   integer, parameter :: initial_size = 8
   integer :: top
   type(toml_key), allocatable :: stack(:)
   type(toml_token), allocatable :: leading_whitespace, trailing_whitespace


   call consume(parser, lexer, token_kind%lbracket)
   if (allocated(parser%diagnostic)) return

   if (parser%token%kind == token_kind%whitespace) then
      leading_whitespace = parser%token
      call next_token(parser, lexer)
   end if

   array_of_tables = parser%token%kind == token_kind%lbracket

   if (array_of_tables .or. parser%token%kind == token_kind%whitespace) then
      call next_token(parser, lexer)
   end if

   call fill_stack(lexer, parser, top, stack)
   if (allocated(parser%diagnostic)) return

   key = stack(top)
   top = top - 1

   call walk_stack(parser, top, stack)

   if (array_of_tables) then
      call parser%current%get(key%key, ptr)
      if (associated(ptr)) then
         array => cast_to_array(ptr)
         if (.not.associated(array)) then
            call duplicate_key_error(parser%diagnostic, lexer, &
               & parser%context%token(key%origin), &
               & parser%context%token(ptr%origin), &
               & "Key '"//key%key//"' already exists")
            return
         end if
         if (array%inline) then
            call semantic_error(parser%diagnostic, lexer, &
               & parser%context%token(key%origin), &
               & parser%context%token(array%origin), &
               & "Array of tables cannot extend inline array", &
               & "extended here", &
               & "defined as inline")
            return
         end if
      else
         call add_array(parser%current, key, array)
         array%inline = .false.
      end if
      call add_table(array, table)
   else
      call parser%current%get(key%key, ptr)
      if (associated(ptr)) then
         table => cast_to_table(ptr)
         if (associated(table)) then
            if (.not.table%implicit) nullify(table)
         end if

         if (.not.associated(table)) then
            call duplicate_key_error(parser%diagnostic, lexer, &
               & parser%context%token(key%origin), &
               & parser%context%token(ptr%origin), &
               & "Key '"//key%key//"' already exists")
            return
         end if
      else
         call add_table(parser%current, key, table)
      end if
   end if

   parser%current => table

   call consume(parser, lexer, token_kind%rbracket)
   if (allocated(parser%diagnostic)) return

   if (array_of_tables) then
      if (parser%token%kind == token_kind%whitespace) then
         trailing_whitespace = parser%token
         call next_token(parser, lexer)
      end if
      call consume(parser, lexer, token_kind%rbracket)
      if (allocated(parser%diagnostic)) return
   end if

   if (array_of_tables .and. allocated(leading_whitespace)) then
      call syntax_error(parser%diagnostic, lexer, leading_whitespace, &
         & "Malformatted array of table header encountered", &
         & "whitespace not allowed in header")
      return
   end if

   if (array_of_tables .and. allocated(trailing_whitespace)) then
      call syntax_error(parser%diagnostic, lexer, trailing_whitespace, &
         & "Malformatted array of table header encountered", &
         & "whitespace not allowed in header")
      return
   end if

   do while(parser%token%kind == token_kind%whitespace)
      call next_token(parser, lexer)
   end do

   if (parser%token%kind == token_kind%comment) then
      call next_token(parser, lexer)
   end if

   if (all(parser%token%kind /= [token_kind%newline, token_kind%eof])) then
      call syntax_error(parser%diagnostic, lexer, parser%token, &
         & "Unexpected "//stringify(parser%token)//" after table header", &
         & "expected newline")
   end if

contains

   !> Fill the stack with tokens
   subroutine fill_stack(lexer, parser, top, stack)
      class(toml_lexer), intent(inout) :: lexer
      type(toml_parser), intent(inout) :: parser
      !> Depth of the table key stack
      integer, intent(out) :: top
      !> Stack of all keys in the table header
      type(toml_key), allocatable, intent(out) :: stack(:)

      top = 0
      allocate(stack(initial_size))

      do
         if (top >= size(stack)) then
            call resize(stack)
         end if

         if (all(parser%token%kind /= [token_kind%string, token_kind%literal, &
            & token_kind%keypath])) then
            call syntax_error(parser%diagnostic, lexer, parser%token, &
               & "Missing key for table header", &
               & "unexpected "//stringify(parser%token))
            return
         end if

         top = top + 1
         call extract_key(parser, lexer, stack(top))

         call next_token(parser, lexer)
         if (parser%token%kind == token_kind%whitespace) &
            & call next_token(parser, lexer)

         if (parser%token%kind == token_kind%rbracket) exit

         call consume(parser, lexer, token_kind%dot)
         if (allocated(parser%diagnostic)) return
         if (parser%token%kind == token_kind%whitespace) &
            & call next_token(parser, lexer)
      end do

      if (top <= 0) then
         call syntax_error(parser%diagnostic, lexer, parser%token, &
            & "Empty table header", &
            & "expected table header")
      end if

   end subroutine fill_stack

   !> Walk the key stack to fetch the correct table, create implicit tables as necessary
   subroutine walk_stack(parser, top, stack)
      type(toml_parser), intent(inout), target :: parser
      !> Depth of the table key stack
      integer, intent(in) :: top
      !> Stack of all keys in the table header
      type(toml_key), intent(in), target :: stack(:)

      type(toml_table), pointer :: table, tmp_tbl
      type(toml_array), pointer :: array
      type(toml_key), pointer :: key
      class(toml_value), pointer :: ptr
      integer :: it

      table => parser%root

      do it = 1, top
         key => stack(it)

         if (.not.table%has_key(key%key)) then
            call add_table(table, key, tmp_tbl)
            if (associated(tmp_tbl)) then
               tmp_tbl%implicit = .true.
            end if
         end if
         call table%get(key%key, ptr)

         table => cast_to_table(ptr)
         if (.not.associated(table)) then
            array => cast_to_array(ptr)
            if (associated(array)) then
               call array%get(len(array), ptr)
               table => cast_to_table(ptr)
            end if
            if (.not.associated(table)) then
               call duplicate_key_error(parser%diagnostic, lexer, &
                  & parser%context%token(key%origin), &
                  & parser%context%token(ptr%origin), &
                  & "Key '"//key%key//"' already exists")
               return
            end if
         end if

         if (table%inline) then
            call semantic_error(parser%diagnostic, lexer, &
               & parser%context%token(key%origin), &
               & parser%context%token(table%origin), &
               & "Inline table '"//key%key//"' cannot be used as a key", &
               & "inline table cannot be extended", &
               & "defined as inline first")
         end if
      end do

      parser%current => table
   end subroutine walk_stack

   !> Change size of the stack
   subroutine resize(stack, n)
      !> Stack of keys to be resized
      type(toml_key), allocatable, intent(inout) :: stack(:)
      !> New size of the stack
      integer, intent(in), optional :: n

      type(toml_key), allocatable :: tmp(:)
      integer :: m

      if (present(n)) then
         m = n
      else
         if (allocated(stack)) then
            m = size(stack)
            m = m + m/2 + 1
         else
            m = initial_size
         end if
      end if

      if (allocated(stack)) then
         call move_alloc(stack, tmp)
         allocate(stack(m))

         m = min(size(tmp), m)
         stack(:m) = tmp(:m)

         deallocate(tmp)
      else
         allocate(stack(m))
      end if
   end subroutine resize

end subroutine parse_table_header

!> Parse key value pairs in a table body
recursive subroutine parse_keyval(parser, lexer, table)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current table
   type(toml_table), intent(inout) :: table

   class(toml_value), pointer :: ptr
   type(toml_keyval), pointer :: vptr
   type(toml_array), pointer :: aptr
   type(toml_table), pointer :: tptr
   type(toml_key) :: key

   call extract_key(parser, lexer, key)
   call next_token(parser, lexer)
   if (parser%token%kind == token_kind%whitespace) &
      call next_token(parser, lexer)

   if (parser%token%kind == token_kind%dot) then
      call get_table(table, key, tptr)
      if (tptr%inline) then
         call semantic_error(parser%diagnostic, lexer, &
            & parser%context%token(key%origin), &
            & parser%context%token(tptr%origin), &
            & "Cannot add keys to inline tables", &
            & "inline table cannot be extended", &
            & "defined as inline first")
         return
      end if

      call next_token(parser, lexer)
      if (parser%token%kind == token_kind%whitespace) &
         call next_token(parser, lexer)

      if (any(parser%token%kind == [token_kind%keypath, token_kind%string, &
         & token_kind%literal])) then
         call parse_keyval(parser, lexer, tptr)
      else
         call syntax_error(parser%diagnostic, lexer, parser%token, &
            & "Invalid syntax", &
            & "expected key")
      end if
      return
   end if

   call consume(parser, lexer, token_kind%equal)
   if (allocated(parser%diagnostic)) return

   if (parser%token%kind == token_kind%whitespace) &
      call next_token(parser, lexer)

   call table%get(key%key, ptr)
   if (associated(ptr)) then
      call duplicate_key_error(parser%diagnostic, lexer, &
         & parser%context%token(key%origin), &
         & parser%context%token(ptr%origin), &
         & "Key '"//key%key//"' already exists")
      return
   end if

   select case(parser%token%kind)
   case default
      call add_keyval(table, key, vptr)
      call parse_value(parser, lexer, vptr)

   case(token_kind%nil)
      call next_token(parser, lexer)

   case(token_kind%lbracket)
      call add_array(table, key, aptr)
      call parse_inline_array(parser, lexer, aptr)

   case(token_kind%lbrace)
      call add_table(table, key, tptr)
      call parse_inline_table(parser, lexer, tptr)

   end select
   if (allocated(parser%diagnostic)) return

   if (parser%token%kind == token_kind%whitespace) &
      call next_token(parser, lexer)

   if (parser%token%kind == token_kind%comment) &
      call next_token(parser, lexer)
end subroutine parse_keyval

recursive subroutine parse_inline_array(parser, lexer, array)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current array
   type(toml_array), intent(inout) :: array

   type(toml_keyval), pointer :: vptr
   type(toml_array), pointer :: aptr
   type(toml_table), pointer :: tptr
   integer, parameter :: skip_tokens(*) = &
      [token_kind%whitespace, token_kind%comment, token_kind%newline]

   array%inline = .true.
   call consume(parser, lexer, token_kind%lbracket)

   inline_array: do while(.not.allocated(parser%diagnostic))
      do while(any(parser%token%kind == skip_tokens))
         call next_token(parser, lexer)
      end do

      select case(parser%token%kind)
      case(token_kind%rbracket)
         exit inline_array

      case default
         call add_keyval(array, vptr)
         call parse_value(parser, lexer, vptr)

      case(token_kind%nil)
         call next_token(parser, lexer)

      case(token_kind%lbracket)
         call add_array(array, aptr)
         call parse_inline_array(parser, lexer, aptr)

      case(token_kind%lbrace)
         call add_table(array, tptr)
         call parse_inline_table(parser, lexer, tptr)

      end select
      if (allocated(parser%diagnostic)) exit inline_array

      do while(any(parser%token%kind == skip_tokens))
         call next_token(parser, lexer)
      end do

      if (parser%token%kind == token_kind%comma) then
         call next_token(parser, lexer)
         cycle inline_array
      end if
      exit inline_array
   end do inline_array
   if (allocated(parser%diagnostic)) return

   call consume(parser, lexer, token_kind%rbracket)
end subroutine parse_inline_array

recursive subroutine parse_inline_table(parser, lexer, table)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current table
   type(toml_table), intent(inout) :: table

   table%inline = .true.
   call consume(parser, lexer, token_kind%lbrace)

   if (parser%token%kind == token_kind%whitespace) &
      call next_token(parser, lexer)

   if (parser%token%kind == token_kind%rbrace) then
      call next_token(parser, lexer)
      return
   end if

   inline_table: do while(.not.allocated(parser%diagnostic))
      if (parser%token%kind == token_kind%whitespace) &
         call next_token(parser, lexer)

      select case(parser%token%kind)
      case default
         call syntax_error(parser%diagnostic, lexer, parser%token, &
            & "Invalid character in inline table", &
            & "unexpected "//stringify(parser%token))

      case(token_kind%keypath, token_kind%string, token_kind%literal)
         call parse_keyval(parser, lexer, table)

      end select
      if (allocated(parser%diagnostic)) exit inline_table

      if (parser%token%kind == token_kind%whitespace) &
         call next_token(parser, lexer)

      if (parser%token%kind == token_kind%comma) then
         call next_token(parser, lexer)
         cycle inline_table
      end if
      if (parser%token%kind == token_kind%rbrace) exit inline_table
   end do inline_table
   if (allocated(parser%diagnostic)) return

   call consume(parser, lexer, token_kind%rbrace)
end subroutine parse_inline_table

subroutine parse_value(parser, lexer, kval)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Current key value pair
   type(toml_keyval), intent(inout) :: kval

   select case(parser%token%kind)
   case default
      call syntax_error(parser%diagnostic, lexer, parser%token, &
         & "Invalid expression for value", &
         & "unexpected "//stringify(parser%token))

   case(token_kind%unclosed)
      ! Handle runaway expressions separately
      call syntax_error(parser%diagnostic, lexer, parser%token, &
         & "Inline expression contains unclosed or runaway group", &
         & "unclosed inline expression")

   case(token_kind%string, token_kind%mstring, token_kind%literal, token_kind%mliteral, &
         & token_kind%int, token_kind%float, token_kind%bool, token_kind%datetime)
      call extract_value(parser, lexer, kval)

      call next_token(parser, lexer)
   end select
end subroutine parse_value

!> Check whether the current token is the expected one and advance the lexer
subroutine consume(parser, lexer, kind)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Expected token kind
   integer, intent(in) :: kind

   if (parser%token%kind /= kind) then
      call syntax_error(parser%diagnostic, lexer, parser%token, &
         & "Invalid syntax in this context", &
         & "expected "//stringify(toml_token(kind)))
      return
   end if

   call next_token(parser, lexer)
end subroutine consume

!> Create diagnostic for invalid syntax
subroutine syntax_error(diagnostic, lexer, token, message, label)
   !> Diagnostic for the syntax error
   type(toml_diagnostic), allocatable, intent(out) :: diagnostic
   !> Instance of the lexer providing the context
   class(toml_lexer), intent(inout) :: lexer
   !> Token that caused the error
   type(toml_token), intent(in) :: token
   !> Message for the error
   character(len=*), intent(in) :: message
   !> Label for the token
   character(len=*), intent(in) :: label

   character(:, tfc), allocatable :: filename

   call lexer%get_info("filename", filename)

   allocate(diagnostic)
   diagnostic = toml_diagnostic( &
      & toml_level%error, &
      & message, &
      & filename, &
      & [toml_label(toml_level%error, token%first, token%last, label, .true.)])
end subroutine syntax_error

!> Create diagnostic for incorrect semantics
subroutine semantic_error(diagnostic, lexer, token1, token2, message, label1, label2)
   !> Diagnostic for the duplicate key error
   type(toml_diagnostic), allocatable, intent(out) :: diagnostic
   !> Instance of the lexer providing the context
   class(toml_lexer), intent(inout) :: lexer
   !> Token identifying the duplicate key
   type(toml_token), intent(in) :: token1
   !> Token identifying the original key
   type(toml_token), intent(in) :: token2
   !> Message for the error
   character(len=*), intent(in) :: message
   !> Label for the first token
   character(len=*), intent(in) :: label1
   !> Label for the second token
   character(len=*), intent(in) :: label2

   character(:, tfc), allocatable :: filename

   call lexer%get_info("filename", filename)

   allocate(diagnostic)
   diagnostic = toml_diagnostic( &
      & toml_level%error, &
      & message, &
      & filename, &
      & [toml_label(toml_level%error, token1%first, token1%last, label1, .true.), &
      &  toml_label(toml_level%info, token2%first, token2%last, label2, .false.)])
end subroutine semantic_error

!> Create a diagnostic for a duplicate key entry
subroutine duplicate_key_error(diagnostic, lexer, token1, token2, message)
   !> Diagnostic for the duplicate key error
   type(toml_diagnostic), allocatable, intent(out) :: diagnostic
   !> Instance of the lexer providing the context
   class(toml_lexer), intent(inout) :: lexer
   !> Token identifying the duplicate key
   type(toml_token), intent(in) :: token1
   !> Token identifying the original key
   type(toml_token), intent(in) :: token2
   !> Message for the error
   character(len=*), intent(in) :: message

   call semantic_error(diagnostic, lexer, token1, token2, &
      & message, "key already used", "first defined here")
end subroutine duplicate_key_error

!> Create an error from a diagnostic
subroutine make_error(error, diagnostic, lexer, color)
   !> Error to be created
   type(toml_error), allocatable, intent(out) :: error
   !> Diagnostic to be used
   type(toml_diagnostic), intent(in) :: diagnostic
   !> Instance of the lexer providing the context
   class(toml_lexer), intent(in) :: lexer
   !> Use colorful error messages
   type(toml_terminal), intent(in) :: color

   character(len=:), allocatable :: str

   allocate(error)
   call lexer%get_info("source", str)
   error%message = render(diagnostic, str, color)
   error%stat = toml_stat%fatal
end subroutine make_error

!> Wrapper around the lexer to retrieve the next token.
!> Allows to record the tokens for keys and values in the parser context
subroutine next_token(parser, lexer)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer

   call lexer%next(parser%token)

   select case(parser%token%kind)
   case(token_kind%keypath, token_kind%string, token_kind%literal, token_kind%int, &
         & token_kind%float, token_kind%bool, token_kind%datetime)
      call parser%context%push_back(parser%token)
   case(token_kind%newline, token_kind%dot, token_kind%comma, token_kind%equal, &
         & token_kind%lbrace, token_kind%rbrace, token_kind%lbracket, token_kind%rbracket)
      if (parser%config%context_detail > 0) &
         call parser%context%push_back(parser%token)
   case default
      if (parser%config%context_detail > 1) &
         call parser%context%push_back(parser%token)
   end select
end subroutine next_token

!> Extract key from token
subroutine extract_key(parser, lexer, key)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Key to be extracted
   type(toml_key), intent(out) :: key

   call lexer%extract(parser%token, key%key)
   key%origin = parser%context%top
   if (scan(key%key, TOML_NEWLINE) > 0) then
      call syntax_error(parser%diagnostic, lexer, parser%token, &
         & "Invalid character in key", &
         & "key cannot contain newline")
      return
   end if
end subroutine extract_key

!> Extract value from token
subroutine extract_value(parser, lexer, kval)
   !> Instance of the parser
   class(toml_parser), intent(inout) :: parser
   !> Instance of the lexer
   class(toml_lexer), intent(inout) :: lexer
   !> Value to be extracted
   type(toml_keyval), intent(inout) :: kval

   character(:, tfc), allocatable :: sval
   real(tfr) :: rval
   integer(tfi) :: ival
   logical :: bval
   type(toml_datetime) :: dval

   kval%origin_value = parser%context%top

   select case(parser%token%kind)
   case(token_kind%string, token_kind%literal, token_kind%mstring, token_kind%mliteral)
      call lexer%extract_string(parser%token, sval)
      call kval%set(sval)

   case(token_kind%int)
      call lexer%extract_integer(parser%token, ival)
      call kval%set(ival)

   case(token_kind%float)
      call lexer%extract_float(parser%token, rval)
      call kval%set(rval)

   case(token_kind%bool)
      call lexer%extract_bool(parser%token, bval)
      call kval%set(bval)

   case(token_kind%datetime)
      call lexer%extract_datetime(parser%token, dval)
      call kval%set(dval)
   end select
end subroutine extract_value

!> Try to retrieve TOML table with key or create it
subroutine get_table(table, key, ptr, stat)
   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table
   !> Key for the new table
   type(toml_key), intent(in) :: key
   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr
   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp

   nullify(ptr)
   call table%get(key%key, tmp)

   if (associated(tmp)) then
      ptr => cast_to_table(tmp)
      if (present(stat)) stat = merge(toml_stat%success, toml_stat%fatal, associated(ptr))
   else
      call add_table(table, key, ptr, stat)
   end if
end subroutine get_table

end module tomlf_de_parser
