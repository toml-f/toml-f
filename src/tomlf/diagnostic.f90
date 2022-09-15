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

!> Diagnostic message support for TOML Fortran
module tomlf_diagnostic
   use tomlf_terminal, only : toml_terminal, ansi_code, operator(//), operator(+)
   implicit none
   private

   public :: render
   public :: toml_diagnostic, toml_label


   interface render
      module procedure render_diagnostic
      module procedure render_text
      module procedure render_text_with_label
      module procedure render_text_with_labels
   end interface render


   !> Enumerator for diagnostic levels
   type :: level_enum
      integer :: error = 0
      integer :: warning = 1
      integer :: help = 2
      integer :: note = 3
      integer :: info = 4
   end type level_enum

   !> Actual enumerator values
   type(level_enum), parameter, public :: toml_level = level_enum()


   type toml_label
      !> Level of message
      integer :: level
      !> Primary message
      logical :: primary
      !> First and last character of message
      integer :: first, last
      !> Message text
      character(len=:), allocatable :: text
      !> Identifier of context
      character(len=:), allocatable :: source
   end type toml_label

   interface toml_label
      module procedure new_label
   end interface toml_label


   !> Definition of diagnostic message
   type :: toml_diagnostic
      !> Level of message
      integer :: level
      !> Primary message
      character(len=:), allocatable :: message
      !> Context of the diagnostic source
      character(len=:), allocatable :: source
      !> Messages associated with this diagnostic
      type(toml_label), allocatable :: label(:)
   end type toml_diagnostic

   interface toml_diagnostic
      module procedure new_diagnostic
   end interface toml_diagnostic


   type :: line_token
      integer :: first, last
   end type line_token

   character(len=*), parameter :: nl = new_line('a')


contains


pure function new_label(level, first, last, text, primary) result(new)
   integer, intent(in) :: level
   integer, intent(in) :: first, last
   character(len=*), intent(in), optional :: text
   logical, intent(in), optional :: primary
   type(toml_label) :: new

   if (present(text)) new%text = text
   new%level = level
   new%first = first
   new%last = last
   if (present(primary)) then
      new%primary = primary
   else
      new%primary = .false.
   end if
end function new_label


!> Create new diagnostic message
pure function new_diagnostic(level, message, source, label) result(new)
   !> Level of message
   integer, intent(in) :: level
   !> Primary message
   character(len=*), intent(in), optional :: message
   !> Context of the diagnostic source
   character(len=*), intent(in), optional :: source
   !> Messages associated with this diagnostic
   type(toml_label), intent(in), optional :: label(:)
   type(toml_diagnostic) :: new

   new%level = level
   if (present(message)) new%message = message
   if (present(source)) new%source = source
   if (present(label)) new%label = label
end function new_diagnostic


pure function line_tokens(input) result(token)
   character(len=*), intent(in) :: input
   type(line_token), allocatable :: token(:)

   integer :: first, last

   first = 1
   last = 1
   allocate(token(0))
   do while (first <= len(input))
      if (input(last:last) /= nl) then
         last = last + 1
         cycle
      end if

      token = [token, line_token(first, last-1)]
      first = last + 1
      last = first
   end do
end function line_tokens

recursive pure function render_diagnostic(diag, input, color) result(string)
   character(len=*), intent(in) :: input
   type(toml_diagnostic), intent(in) :: diag
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   string = &
      render_message(diag%level, diag%message, color)

   if (allocated(diag%label)) then
      string = string // nl // &
         render_text_with_labels(input, diag%label, color, source=diag%source)
   end if
end function render_diagnostic

pure function render_message(level, message, color) result(string)
   integer, intent(in) :: level
   character(len=*), intent(in), optional :: message
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   if (present(message)) then
      string = &
         level_name(level, color) // color%bold // ": " // message // color%reset
   else
      string = &
         level_name(level, color)
   end if
end function render_message

pure function level_name(level, color) result(string)
   integer, intent(in) :: level
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   select case(level)
   case(toml_level%error)
      string = color%bold + color%red // "error" // color%reset
   case(toml_level%warning)
      string = color%bold + color%yellow // "warning" // color%reset
   case(toml_level%help)
      string = color%bold + color%cyan // "help" // color%reset
   case(toml_level%note)
      string = color%bold + color%blue // "note" // color%reset
   case(toml_level%info)
      string = color%bold + color%magenta // "info" // color%reset
   case default
      string = color%bold + color%blue // "unknown" // color%reset
   end select
end function level_name

pure function render_source(source, offset, color) result(string)
   character(len=*), intent(in) :: source
   integer, intent(in) :: offset
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   string = &
      & repeat(" ", offset) // (color%bold + color%blue) // "-->" // color%reset // " " // source
end function render_source

function render_text(input, color, source) result(string)
   character(len=*), intent(in) :: input
   type(toml_terminal), intent(in) :: color
   character(len=*), intent(in), optional :: source
   character(len=:), allocatable :: string

   integer :: it, offset
   type(line_token), allocatable :: token(:)

   allocate(token(0))  ! avoid compiler warning
   token = line_tokens(input)
   offset = integer_width(size(token))

   if (present(source)) then
      string = render_source(source, offset, color) // nl // &
         & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset
   else
      string = &
         & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset
   end if

   do it = 1, size(token)
      string = string // nl //&
         & render_line(input(token(it)%first:token(it)%last), to_string(it, offset), color)
   end do
   string = string // nl // &
      repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset

end function render_text

function render_text_with_label(input, label, color, source) result(string)
   character(len=*), intent(in) :: input
   type(toml_label), intent(in) :: label
   type(toml_terminal), intent(in) :: color
   character(len=*), intent(in), optional :: source
   character(len=:), allocatable :: string

   integer :: it, offset, first, last, line, shift
   type(line_token), allocatable :: token(:)

   allocate(token(0))  ! avoid compiler warning
   token = line_tokens(input)
   line = count(token%first < label%first)
   associate(first => token%first)
      shift = first(line) - 1
   end associate
   first = max(1, line - 1)
   last = min(size(token), line + 1)
   offset = integer_width(last)

   if (present(source)) then
      string = render_source(source, offset, color) // ":" // &
         & to_string(line) // ":" // &
         & to_string(label%first)
      if (label%first /= label%last) then
         string = string // "-" // to_string(label%last)
      end if
   end if
   string = string // nl // &
      & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset

   do it = first, last
      string = string // nl //&
         & render_line(input(token(it)%first:token(it)%last), &
         &             to_string(it, offset), color)
      if (it == line) then
         string = string // nl //&
            & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset // &
            & render_label(label, shift, color)
      end if
   end do
   string = string // nl // &
      repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset

end function render_text_with_label

pure function render_text_with_labels(input, label, color, source) result(string)
   character(len=*), intent(in) :: input
   type(toml_label), intent(in) :: label(:)
   type(toml_terminal), intent(in) :: color
   character(len=*), intent(in), optional :: source
   character(len=:), allocatable :: string

   integer :: it, il, offset, first, last, line(size(label)), shift(size(label))
   type(line_token), allocatable :: token(:)
   logical, allocatable :: display(:)

   allocate(token(0))  ! avoid compiler warning
   token = line_tokens(input)
   line(:) = [(count(token%first <= label(it)%first), it = 1, size(label))]
   associate(first => token%first)
      shift(:) = first(line) - 1
   end associate
   first = max(1, minval(line))
   last = min(size(token), maxval(line))
   offset = integer_width(last)

   it = 1  ! Without a primary we use the first label
   do il = 1, size(label)
      if (label(il)%primary) then
         it = il
         exit
      end if
   end do

   if (present(source)) then
      string = render_source(source, offset, color) // ":" // &
         & to_string(line(it)) // ":" // &
         & to_string(label(it)%first-shift(it))
      if (label(it)%first /= label(it)%last) then
         string = string // "-" // to_string(label(it)%last-shift(it))
      end if
   end if
   string = string // nl // &
      & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset

   allocate(display(first:last), source=.false.)
   do il = 1, size(label)
      ! display(max(first, line(il) - 1):min(last, line(il) + 1)) = .true.
      display(line(il)) = .true.
   end do

   do it = first, last
      if (.not.display(it)) then
         if (display(it-1) .and. count(display(it:)) > 0) then
            string = string // nl //&
               & repeat(" ", offset + 1) // (color%bold + color%blue) // ":" // color%reset
         end if
         cycle
      end if

      string = string // nl //&
         & render_line(input(token(it)%first:token(it)%last), &
         &             to_string(it, offset), color)
      if (any(it == line)) then
         do il = 1, size(label)
            if (line(il) /= it) cycle
            string = string // nl //&
               & repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset // &
               & render_label(label(il), shift(il), color)
         end do
      end if
   end do
   string = string // nl // &
      repeat(" ", offset + 1) // (color%bold + color%blue) // "|" // color%reset

end function render_text_with_labels

pure function render_label(label, shift, color) result(string)
   type(toml_label), intent(in) :: label
   integer, intent(in) :: shift
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   integer :: width
   character :: marker
   type(ansi_code) :: this_color

   marker = merge("^", "-", label%primary)
   width = label%last - label%first + 1
   this_color = level_color(label%level, color)

   string = &
      & repeat(" ", label%first - shift) // this_color // repeat(marker, width) // color%reset
   if (allocated(label%text)) then
      string = string // &
         & " " // this_color // label%text // color%reset
   end if

end function render_label

pure function level_color(level, color) result(this_color)
   integer, intent(in) :: level
   type(toml_terminal), intent(in) :: color
   type(ansi_code) :: this_color

   select case(level)
   case(toml_level%error)
      this_color = color%bold + color%red
   case(toml_level%warning)
      this_color = color%bold + color%yellow
   case(toml_level%help)
      this_color = color%bold + color%cyan
   case(toml_level%info)
      this_color = color%bold + color%magenta
   case default
      this_color = color%bold + color%blue
   end select
end function level_color

pure function render_line(input, line, color) result(string)
   character(len=*), intent(in) :: input
   character(len=*), intent(in) :: line
   type(toml_terminal), intent(in) :: color
   character(len=:), allocatable :: string

   string = &
      & line // " " // (color%bold + color%blue) // "|" // color%reset // " " // input
end function render_line

pure function integer_width(input) result(width)
   integer, intent(in) :: input
   integer :: width

   integer :: val

   val = input
   width = 0
   do while (val /= 0)
      val = val / 10
      width = width + 1
   end do

end function integer_width

!> Represent an integer as character sequence.
pure function to_string(val, width) result(string)
   integer, intent(in) :: val
   integer, intent(in), optional :: width
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10))
      n = n/10
   end do
   if (val < 0) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   if (present(width)) then
      string = repeat(" ", max(width-(buffer_len+1-pos), 0)) // buffer(pos:)
   else
      string = buffer(pos:)
   end if
end function to_string


end module tomlf_diagnostic
