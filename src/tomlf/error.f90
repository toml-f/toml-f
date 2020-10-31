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

!> Central registry for error codes
module tomlf_error
   use tomlf_constants, only : tfc, TOML_NEWLINE
   implicit none
   private

   public :: toml_stat, toml_error, toml_context
   public :: syntax_error, duplicate_key_error, io_error, vendor_error


   !> Possible TOML-Fortran error codes
   type :: enum_stat

      !> Successful run
      integer :: success = 0

      !> Internal error:
      !
      !  General undefined error state, usually caused by algorithmic errors.
      integer :: fatal = -1

      !> Duplicate key error:
      !
      !  Tried to push back an already present key on a TOML table or
      !  TOML document contains duplicate keys, already present in table.
      integer :: duplicate_key = 1

      !> Syntax error
      integer :: invalid_syntax = 2

      !> IO error
      integer :: io_failure = 3

   end type enum_stat

   !> Actual enumerator for return states
   type(enum_stat), parameter :: toml_stat = enum_stat()


   !> Context for error message (usually a line in a TOML document)
   type :: toml_context

      !> Current internal position
      integer :: pos = 0

      !> Current internal count
      integer :: num = 0

      !> Current internal location on the string buffer
      character(kind=tfc, len=:), pointer :: ptr => null()

   end type toml_context


   !> Error message produced by TOML-Fortran
   type :: toml_error

      !> Error code
      integer :: stat = toml_stat%fatal

      !> Payload of the error
      character(kind=tfc, len=:), allocatable :: message

   end type toml_error


contains


!> A syntactic error in a TOML document was found
subroutine syntax_error(error, context, message, stat)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Current context producing the error
   type(toml_context), intent(in), optional :: context

   !> A detailed message describing the error and (optionally) offering advice
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Overwrite of the error code
   integer, intent(in), optional :: stat

   allocate(error)

   if (present(stat)) then
      error%stat = stat
   else
      error%stat = toml_stat%invalid_syntax
   end if

   if (present(message)) then
      error%message = message
   else
      error%message = "Syntax error"
   end if

   if (present(context)) then
      call add_context(error%message, context)
   end if

end subroutine syntax_error


!> Key is present multiple times in a TOML document within the same table
subroutine duplicate_key_error(error, context, key, stat)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Current context producing the error
   type(toml_context), intent(in), optional :: context

   !> The offending duplicate key
   character(kind=tfc, len=*), intent(in), optional :: key

   !> Overwrite of the error code
   integer, intent(in), optional :: stat

   allocate(error)

   if (present(stat)) then
      error%stat = stat
   else
      error%stat = toml_stat%duplicate_key
   end if

   if (present(key)) then
      error%message = "Duplicate key ("//key//") found"
   else
      error%message = "Duplicate key found"
   end if

   if (present(context)) then
      call add_context(error%message, context)
   end if

end subroutine duplicate_key_error


!> IO runtime error
subroutine io_error(error, message)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> A detailed message describing the error and (optionally) offering advice
   character(kind=tfc, len=*), intent(in), optional :: message

   allocate(error)
   error%stat = toml_stat%io_failure

   if (present(message)) then
      error%message = message
   else
      error%message = "IO runtime error"
   end if

end subroutine io_error


!> A shortcoming in the implementation or an internal error occured, rather
!  than falling back to unpredictable and possibly harmful behaviour, we try
!  to offer an apology for this inconvenience
subroutine vendor_error(error, context, message, stat)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Current context producing the error
   type(toml_context), intent(in), optional :: context

   !> A detailed message describing the error and (optionally) offering advice
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Overwrite of the error code
   integer, intent(in), optional :: stat

   allocate(error)

   if (present(stat)) then
      error%stat = stat
   else
      error%stat = toml_stat%fatal
   end if

   if (present(message)) then
      error%message = message
   else
      error%message = "Internal error"
   end if

   if (present(context)) then
      call add_context(error%message, context)
   end if

end subroutine vendor_error


!> Put an existing error message into a more useful context
subroutine add_context(message, context)

   !> A detailed message describing the error, requiring some more context
   character(len=:), allocatable, intent(inout) :: message

   !> Current context producing the error
   type(toml_context), intent(in) :: context

   character(len=20) :: num
   integer :: line_break

   if (context%num > 0) then
      write(num, '("line",1x,i0,":")') context%num
      message = num(1:len_trim(num)+1) // message
   end if

   if (associated(context%ptr)) then
      line_break = index(context%ptr, TOML_NEWLINE)-1
      if (line_break < 0) line_break = len(context%ptr)
      message = message // TOML_NEWLINE // &
         & '   | '// context%ptr(1:line_break) // TOML_NEWLINE // &
         & '   |'
      if (context%pos > 0 .and. context%pos <= line_break) then
         message = message // repeat('-', context%pos) // '^'
      end if
   end if

end subroutine add_context


end module tomlf_error
