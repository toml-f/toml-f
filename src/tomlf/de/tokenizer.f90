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

!> Definition of the TOML tokens and the possible states of the tokenizer
!>
!> The tokenizer implementation has to produce tokens from any input source
!> and is usually only required for string tokens to provide an actual character
!> representation.
!>
!> The tokenization is partly dependent on the context, as the dot is not in
!> all states actually a token, also due to the rather complex syntax of
!> table headers, whitespace is precious and has to be reported as token.
!>
!> Not required but usually helpful is the creation of a context, usually
!> represented by the current line (or a chunk of lines for multiline strings),
!> which can be passed to the error handler to create more detailed output.
!> A tokenizer working with the complete TOML document as character sequence
!> can easily create the context, while it might be incomplete or missing in
!> case of a stream processing.
module tomlf_de_tokenizer
   use tomlf_constants, only : toml_escape, tfc, TOML_BAREKEY, toml_type
   use tomlf_error, only : toml_stat, toml_error, toml_context, &
      & syntax_error, duplicate_key_error, vendor_error
   use tomlf_utils
   use tomlf_type, only : toml_value, toml_key, toml_table, toml_array, &
      & toml_keyval, new_table, add_array, add_table, add_keyval, len
   implicit none
   private

   public :: toml_tokenizer, toml_token, toml_tokentype


   type :: enum_tokentype

      integer :: invalid = 0

      integer :: dot = 1

      integer :: comma = 2

      integer :: equal = 3

      integer :: lbrace = 4

      integer :: rbrace = 5

      integer :: whitespace = 6

      integer :: newline = 7

      integer :: lbracket = 8

      integer :: rbracket = 9

      integer :: string = 10

      integer :: comment = -1

   end type enum_tokentype

   type(enum_tokentype), parameter :: toml_tokentype = enum_tokentype()


   !> Basic TOML token, produced by a TOML tokenizer
   type :: toml_token

      !> Actual tokentype
      integer :: tok = toml_tokentype%invalid

      !> Character representation of the token
      character(len=:), pointer :: ptr => null()

      !> Length of the token at ptr
      integer :: len = 0

   end type toml_token


   !> Abstract TOML tokenizer
   type, abstract :: toml_tokenizer

      !> Signals if the tokenizer has finished (EOF has been reached)
      logical :: finished = .false.

      !> Current token
      type(toml_token) :: tok

      !> Root table
      type(toml_table), allocatable :: root

      !> Pointer to the current table while transversing a table path
      type(toml_table), pointer :: current => null()

      !> Current line (for error handling)
      type(toml_context) :: line

      !> Error buffer, if allocated an error has occurred
      type(toml_error), allocatable :: error

   contains

      !> Entry point for parsing the TOML document, creates the root table
      procedure :: parse => parse_root

      !> Parse a TOML table or array of tables header
      procedure, private :: parse_select

      !> Parse an inline TOML array
      procedure, private :: parse_array

      !> Parse an inline TOML table
      procedure, private :: parse_table

      !> Parse a key-value pair
      procedure, private :: parse_keyval

      !> Advance tokenizer
      procedure, private :: next

      !> Return next token
      procedure(next_token), deferred :: next_token

   end type toml_tokenizer


   abstract interface
      !> Return next token
      subroutine next_token(de, dot_is_token)
         import :: toml_tokenizer

         !> Instance of the tokenizer
         class(toml_tokenizer), intent(inout) :: de

         !> Dot should be handled as token
         logical, intent(in) :: dot_is_token
      end subroutine next_token
   end interface


contains


!> Entry point for parsing the TOML document, creates the root table
subroutine parse_root(de)

   !> Instance of the TOML deserializer
   class(toml_tokenizer), intent(inout), target :: de

   allocate(de%root)
   call new_table(de%root)
   de%current => de%root

   do while(.not.de%finished)
      select case(de%tok%tok)
      case default
         call syntax_error(de%error, de%line, "syntax error")
         exit

      case(toml_tokentype%newline)
         call de%next(.true.)

      case(toml_tokentype%string)
         call de%parse_keyval(de%current)
         if (allocated(de%error)) exit
         if (de%tok%tok /= toml_tokentype%newline) then
            call syntax_error(de%error, de%line, "extra characters after value present")
            exit
         end if

      case(toml_tokentype%lbracket)
         call parse_select(de)
         if (allocated(de%error)) exit

      end select
   end do

end subroutine parse_root


!> Parse a key-value pair
recursive subroutine parse_keyval(de, table)

   !> Instance of the TOML deserializer
   class(toml_tokenizer), intent(inout), target :: de

   !> Current TOML table
   type(toml_table), intent(inout) :: table

   type(toml_token) :: key
   type(toml_keyval), pointer :: vptr
   type(toml_array), pointer :: aptr
   type(toml_table), pointer :: tptr
   character(kind=tfc, len=:), allocatable :: new_key, this_key

   key = de%tok
   !@:ASSERT(de%tok%tok == STRING)
   call de%next(.true.)

   if (de%tok%tok == toml_tokentype%dot) then
      ! create new key from token
      call key_from_token(this_key, key)
      call get_table(table, this_key, tptr)
      deallocate(this_key)
      if (tptr%inline) then
         call syntax_error(de%error, de%line, "Cannot add keys to inline tables")
         return
      end if
      call de%next(.true.)
      if (de%tok%tok == toml_tokentype%string) then
         call de%parse_keyval(tptr)
      else
         call syntax_error(de%error, de%line, "invalid key")
      end if
      return
   end if

   if (de%tok%tok /= toml_tokentype%equal) then
      call syntax_error(de%error, de%line, "missing =")
      return
   end if

   call de%next(.false.)
   if (allocated(de%error)) return

   ! create new key from token
   call key_from_token(new_key, key)
   if (.not.allocated(new_key)) then
      call syntax_error(de%error, de%line, "invalid key")
      return
   end if

   select case(de%tok%tok)
   case default
      call syntax_error(de%error, de%line, "unexpected token")
      return

   case(toml_tokentype%string) ! key = "value"
      call add_keyval(table, new_key, vptr)
      if (.not.associated(vptr)) then
         call duplicate_key_error(de%error, de%line, new_key)
         return
      end if
      vptr%raw = de%tok%ptr(:de%tok%len)
      if (toml_get_value_type(vptr%raw) == toml_type%invalid) then
         call syntax_error(de%error, de%line, "unknown value type")
         return
      end if
      call de%next(.true.)
      if (allocated(de%error)) return

   case(toml_tokentype%lbracket) ! key = [ array ]
      call add_array(table, new_key, aptr)
      if (.not.associated(aptr)) then
         call duplicate_key_error(de%error, de%line, new_key)
         return
      end if
      aptr%inline = .true.
      call de%parse_array(aptr)
      if (allocated(de%error)) return

   case(toml_tokentype%lbrace) ! key = { table }
      call add_table(table, new_key, tptr)
      if (.not.associated(tptr)) then
         call duplicate_key_error(de%error, de%line, new_key)
         return
      end if
      call de%parse_table(tptr)
      tptr%inline = .true.
      if (allocated(de%error)) return

   end select

end subroutine parse_keyval


!> Parse a TOML table or array of tables header
subroutine parse_select(de)

   !> Instance of the TOML deserializer
   class(toml_tokenizer), intent(inout), target :: de

   type(toml_array), pointer :: array
   type(toml_table), pointer :: table
   class(toml_value), pointer :: ptr
   character(kind=tfc, len=:), allocatable :: key
   logical :: llb

   integer, parameter :: initial_size = 8

   integer :: top
   type(toml_key), allocatable :: stack(:)

   nullify(table)

   !@:assert(de%tok%tok == toml_tokentype%lbracket)
   call de%next(.true., whitespace_is_precious=.true.)

   llb = de%tok%tok == toml_tokentype%lbracket

   if (llb .or. de%tok%tok == toml_tokentype%whitespace) then
      call de%next(.true.)
   end if

   call fill_stack(de, top, stack)
   if (allocated(de%error)) return

   ! remove topmost element from path
   call move_alloc(stack(top)%key, key)
   top = top - 1

   call walk_stack(de, top, stack)
   if (allocated(de%error)) return

   if (llb) then
      ! [[key.key.top]]
      call de%current%get(key, ptr)
      if (associated(ptr)) then
         select type(ptr)
         type is(toml_array)
            array => ptr
         class default
            call duplicate_key_error(de%error, de%line, key)
            return
         end select
      else
         call add_array(de%current, key, array)
         array%inline = .false.
      end if
      if (array%inline) then
         call syntax_error(de%error, de%line, "Cannot use inline array in array of tables")
         return
      end if
      call add_table(array, table)
   else
      ! [key.key.top]
      call de%current%get(key, ptr)
      if (associated(ptr)) then
         select type(ptr)
         type is(toml_table)
            if (ptr%implicit) then
               table => ptr
            else
               call duplicate_key_error(de%error, de%line, key)
               return
            end if
         class default
            call duplicate_key_error(de%error, de%line, key)
            return
         end select
      else
         call add_table(de%current, key, table)
      end if
   end if

   if (.not.associated(table)) then
      call syntax_error(de%error, de%line, "Cannot add table in this context")
      return
   end if
   de%current => table

   if (de%tok%tok /= toml_tokentype%rbracket) then
      call syntax_error(de%error, de%line, "expects ]")
      return
   end if
   call de%next(.true., whitespace_is_precious=llb)
   if (llb) then
      if (de%tok%tok /= toml_tokentype%rbracket) then
         call syntax_error(de%error, de%line, "expects ]]")
         return
      end if
      call de%next(.true.)
   end if

   if (de%tok%tok /= toml_tokentype%newline) then
      call syntax_error(de%error, de%line, "extra chars after ] or ]]")
      return
   end if


contains


   !> Fill the stack with tokens
   subroutine fill_stack(de, top, stack)

      !> Instance of the TOML deserializer
      class(toml_tokenizer), intent(inout), target :: de

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

         if (de%tok%tok /= toml_tokentype%string) then
            call syntax_error(de%error, de%line, "invalid or missing key")
            return
         end if

         top = top + 1
         call key_from_token(stack(top)%key, de%tok)
         if (.not.allocated(stack(top)%key)) then
            call syntax_error(de%error, de%line, "invalid key")
            return
         end if

         call de%next(.true.)

         if (de%tok%tok == toml_tokentype%rbracket) exit

         if (de%tok%tok /= toml_tokentype%dot) then
            call syntax_error(de%error, de%line, "invalid key")
            return
         end if

         call de%next(.true.)
      end do

      if (top <= 0) then
         call syntax_error(de%error, de%line, "empty table selector")
      end if

   end subroutine fill_stack


   !> Walk the key stack to fetch the correct table, create implicit tables as
   !  necessary
   subroutine walk_stack(de, top, stack)

      !> Instance of the TOML deserializer
      class(toml_tokenizer), intent(inout), target :: de

      !> Depth of the table key stack
      integer, intent(in) :: top

      !> Stack of all keys in the table header
      type(toml_key), intent(in), target :: stack(:)

      type(toml_table), pointer :: table, tmp_tbl
      character(kind=tfc, len=:), pointer :: key
      class(toml_value), pointer :: ptr, tmp
      integer :: i

      table => de%root

      do i = 1, top
         key => stack(i)%key

         if (.not.table%has_key(key)) then
            call add_table(table, key, tmp_tbl)
            if (associated(tmp_tbl)) then
               tmp_tbl%implicit = .true.
            end if
         end if
         call table%get(key, ptr)

         select type(ptr)
         type is(toml_table)
            table => ptr

         type is(toml_array)
            call ptr%get(len(ptr), tmp)
            select type(tmp)
            type is(toml_table)
               table => tmp
            class default
               call vendor_error(de%error, de%line)
               return
            end select

         class default
            call duplicate_key_error(de%error, de%line, key)
            return
         end select
      end do

      de%current => table

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


end subroutine parse_select


!> Parse an inline TOML array
recursive subroutine parse_array(de, array)

   !> Instance of the TOML deserializer
   class(toml_tokenizer), intent(inout), target :: de

   !> TOML array to be filled
   type(toml_array), intent(inout) :: array

   type(toml_table), pointer :: tbl
   type(toml_keyval), pointer :: val
   type(toml_array), pointer :: arr

   !@:assert(de%tok%tok == toml_tokentype%lbracket)

   call de%next(.false.)
   do while(.not.allocated(de%error))
      do while(de%tok%tok == toml_tokentype%newline)
         call de%next(.false.)
      end do
      if (de%tok%tok == toml_tokentype%rbracket) then
         exit
      end if

      select case(de%tok%tok)
      case default
         call syntax_error(de%error, de%line, "unexpected token")
         return

      case(toml_tokentype%string) ! [ value, value ... ]
         call add_keyval(array, val)
         val%raw = de%tok%ptr(:de%tok%len)

         call de%next(.false.)

      case(toml_tokentype%lbracket) ! [ [array], [array] ...]
         call add_array(array, arr)
         arr%inline = .true.
         call de%parse_array(arr)
         if (allocated(de%error)) return

      case(toml_tokentype%lbrace) ! [ {table}, {table} ... ]
         call add_table(array, tbl)
         tbl%inline = .true.
         call de%parse_table(tbl)
         if (allocated(de%error)) return

      end select

      do while(de%tok%tok == toml_tokentype%newline)
         call de%next(.false.)
      end do

      if (de%tok%tok == toml_tokentype%comma) then
         call de%next(.false.)
         cycle
      end if
      exit
   end do

   if (de%tok%tok /= toml_tokentype%rbracket) then
      call syntax_error(de%error, de%line, "expects ]")
      return
   end if

   call de%next(.true.)

end subroutine parse_array


!> Parse an inline TOML table
recursive subroutine parse_table(de, table)

   !> Instance of the TOML deserializer
   class(toml_tokenizer), intent(inout), target :: de

   !> TOML table to be filled
   type(toml_table), intent(inout) :: table

   !@:ASSERT(de%tok%tok == LBRACE)
   call de%next(.true.)
   do
      if (de%tok%tok == toml_tokentype%newline) then
         call syntax_error(de%error, de%line, "newline not allowed in inline table")
         return
      end if

      if (de%tok%tok == toml_tokentype%rbrace) exit

      if (de%tok%tok /= toml_tokentype%string) then
         call syntax_error(de%error, de%line, "expects string value")
         return
      end if

      call de%parse_keyval(table)
      if (allocated(de%error)) exit

      if (de%tok%tok == toml_tokentype%string) then
         call syntax_error(de%error, de%line, "newline not allowed in inline table")
         return
      end if

      if (de%tok%tok == toml_tokentype%comma) then
         call de%next(.true.)
         cycle
      end if
      exit
   end do

   if (de%tok%tok /= toml_tokentype%rbrace) then
      call syntax_error(de%error, de%line, "expects }")
      return
   end if

   call de%next(.true.)

end subroutine parse_table


!> Generate a key
subroutine key_from_token(key, tok)

   !> TOML raw key
   character(kind=tfc, len=:), allocatable, intent(out) :: key

   !> String token containing the possible key
   type(toml_token), intent(in) :: tok

   if (toml_raw_to_string(tok%ptr(:tok%len), key)) then
      if (index(key, toml_escape%newline) > 0) deallocate(key)
   else
      key = tok%ptr(:tok%len)
      if (verify(key, TOML_BAREKEY) > 0) deallocate(key)
   end if

end subroutine key_from_token


!> Try to retrieve TOML table with key or create it
subroutine get_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_table)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      call add_table(table, key, ptr, stat)
   end if

end subroutine get_table


!> Return next token
subroutine next(de, dot_is_token, whitespace_is_precious)

   !> Instance of the tokenizer
   class(toml_tokenizer), intent(inout) :: de

   !> Dot should be handled as token
   logical, intent(in) :: dot_is_token

   !> Whitespace tokens should be skipped
   logical, intent(in), optional :: whitespace_is_precious

   logical :: skip_whitespace

   if (present(whitespace_is_precious)) then
      skip_whitespace = .not.whitespace_is_precious
   else
      skip_whitespace = .true.
   end if

   call de%next_token(dot_is_token)
   if (skip_whitespace) then
      do while(de%tok%tok == toml_tokentype%whitespace)
         if (allocated(de%error)) exit
         call de%next_token(dot_is_token)
      end do
   end if

end subroutine next


end module tomlf_de_tokenizer
