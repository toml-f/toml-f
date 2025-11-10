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

!> Implementation of a terminal to provide ANSI escape sequences
!>
!> ANSI escape codes for producing terminal colors. The `ansi_code` derived
!> type is used to store ANSI escape codes and can be combined with other
!> codes or applied to strings by concatenation. The default or uninitialized
!> `ansi_code` is a stub and does not produce escape sequences when applied
!> to a string.
!>
!> Available colors are
!>
!> color          | foreground            | background
!> -------------- | --------------------- | ------------------------
!> black          | `black` (30)          | `bg_black` (40)
!> red            | `red` (31)            | `bg_red` (41)
!> green          | `green` (32)          | `bg_green` (42)
!> yellow         | `yellow` (33)         | `bg_yellow` (43)
!> blue           | `blue` (34)           | `bg_blue` (44)
!> magenta        | `magenta` (35)        | `bg_magenta` (45)
!> cyan           | `cyan` (36)           | `bg_cyan` (46)
!> white          | `white` (37)          | `bg_white` (47)
!> gray           | `gray` (90)           | `bg_gray` (100)
!> bright red     | `bright_red` (91)     | `bg_bright_red` (101)
!> bright green   | `bright_green` (92)   | `bg_bright_green` (102)
!> bright yellow  | `bright_yellow` (93)  | `bg_bright_yellow` (103)
!> bright blue    | `bright_blue` (94)    | `bg_bright_blue` (104)
!> bright magenta | `bright_magenta` (95) | `bg_bright_magenta` (105)
!> bright cyan    | `bright_cyan` (96)    | `bg_bright_cyan` (106)
!> bright white   | `bright_white` (97)   | `bg_bright_white` (107)
!>
!> Available styles are
!>
!> style       |
!> ------------| ---------------
!> reset       | `reset` (0)
!> bold        | `bold` (1)
!> dim         | `dim` (2)
!> italic      | `italic` (3)
!> underline   | `underline` (4)
!> blink       | `blink` (5)
!> blink rapid | `blink_rapid` (6)
!> reverse     | `reverse` (7)
!> hidden      | `hidden` (8)
!> crossed     | `crossed` (9)
module tomlf_terminal
   use tomlf_utils, only : to_string
   implicit none
   private

   public :: toml_terminal
   public :: ansi_code, escape, operator(+), operator(//)


   !> Char length for integers
   integer, parameter :: i1 = selected_int_kind(2)

   !> Container for terminal escape code
   type :: ansi_code
      private
      !> Style descriptor
      integer(i1) :: style = -1_i1
      !> Background color descriptor
      integer(i1) :: bg = -1_i1
      !> Foreground color descriptor
      integer(i1) :: fg = -1_i1
   end type

   interface operator(+)
      module procedure :: add
   end interface operator(+)

   interface operator(//)
      module procedure :: concat_left
      module procedure :: concat_right
   end interface operator(//)

   interface escape
      module procedure :: escape
   end interface escape

   type(ansi_code), public, parameter :: &
      reset = ansi_code(style=0_i1), &
      bold = ansi_code(style=1_i1), &
      dim = ansi_code(style=2_i1), &
      italic = ansi_code(style=3_i1), &
      underline = ansi_code(style=4_i1), &
      blink = ansi_code(style=5_i1), &
      blink_rapid = ansi_code(style=6_i1), &
      reverse = ansi_code(style=7_i1), &
      hidden = ansi_code(style=8_i1), &
      crossed = ansi_code(style=9_i1)

   type(ansi_code), public, parameter :: &
      black = ansi_code(fg=30_i1), &
      red = ansi_code(fg=31_i1), &
      green = ansi_code(fg=32_i1), &
      yellow = ansi_code(fg=33_i1), &
      blue = ansi_code(fg=34_i1), &
      magenta = ansi_code(fg=35_i1), &
      cyan = ansi_code(fg=36_i1), &
      white = ansi_code(fg=37_i1), &
      gray = ansi_code(fg=90_i1), &
      bright_red = ansi_code(fg=91_i1), &
      bright_green = ansi_code(fg=92_i1), &
      bright_yellow = ansi_code(fg=93_i1), &
      bright_blue = ansi_code(fg=94_i1), &
      bright_magenta = ansi_code(fg=95_i1), &
      bright_cyan = ansi_code(fg=96_i1), &
      bright_white = ansi_code(fg=97_i1)

   type(ansi_code), public, parameter :: &
      bg_black = ansi_code(bg=40_i1), &
      bg_red = ansi_code(bg=41_i1), &
      bg_green = ansi_code(bg=42_i1), &
      bg_yellow = ansi_code(bg=43_i1), &
      bg_blue = ansi_code(bg=44_i1), &
      bg_magenta = ansi_code(bg=45_i1), &
      bg_cyan = ansi_code(bg=46_i1), &
      bg_white = ansi_code(bg=47_i1), &
      bg_gray = ansi_code(bg=100_i1), &
      bg_bright_red = ansi_code(bg=101_i1), &
      bg_bright_green = ansi_code(bg=102_i1), &
      bg_bright_yellow = ansi_code(bg=103_i1), &
      bg_bright_blue = ansi_code(bg=104_i1), &
      bg_bright_magenta = ansi_code(bg=105_i1), &
      bg_bright_cyan = ansi_code(bg=106_i1), &
      bg_bright_white = ansi_code(bg=107_i1)


   !> Terminal wrapper to handle color escape sequences, must be initialized with
   !> color support to provide colorful output. Default and uninitialized instances
   !> will remain usable but provide only stubs and do not produce colorful output.
   !> This behavior is useful for creating applications which can toggle color support.
   type :: toml_terminal
      type(ansi_code) :: &
         reset = ansi_code(), &
         bold = ansi_code(), &
         dim = ansi_code(), &
         italic = ansi_code(), &
         underline = ansi_code(), &
         blink = ansi_code(), &
         blink_rapid = ansi_code(), &
         reverse = ansi_code(), &
         hidden = ansi_code(), &
         crossed = ansi_code()

      type(ansi_code) :: &
         black = ansi_code(), &
         red = ansi_code(), &
         green = ansi_code(), &
         yellow = ansi_code(), &
         blue = ansi_code(), &
         magenta = ansi_code(), &
         cyan = ansi_code(), &
         white = ansi_code(), &
         gray = ansi_code(), &
         bright_red = ansi_code(), &
         bright_green = ansi_code(), &
         bright_yellow = ansi_code(), &
         bright_blue = ansi_code(), &
         bright_magenta = ansi_code(), &
         bright_cyan = ansi_code(), &
         bright_white = ansi_code()

      type(ansi_code) :: &
         bg_black = ansi_code(), &
         bg_red = ansi_code(), &
         bg_green = ansi_code(), &
         bg_yellow = ansi_code(), &
         bg_blue = ansi_code(), &
         bg_magenta = ansi_code(), &
         bg_cyan = ansi_code(), &
         bg_white = ansi_code(), &
         bg_gray = ansi_code(), &
         bg_bright_red = ansi_code(), &
         bg_bright_green = ansi_code(), &
         bg_bright_yellow = ansi_code(), &
         bg_bright_blue = ansi_code(), &
         bg_bright_magenta = ansi_code(), &
         bg_bright_cyan = ansi_code(), &
         bg_bright_white = ansi_code()
   end type toml_terminal

   !> Constructor to create new terminal
   interface toml_terminal
      module procedure :: new_terminal
   end interface toml_terminal

contains

!> Create new terminal
pure function new_terminal(use_color) result(new)
   !> Enable color support in terminal
   logical, intent(in) :: use_color
   !> New terminal instance
   type(toml_terminal) :: new

   if (use_color) then
      new%reset = reset
      new%bold = bold
      new%dim = dim
      new%italic = italic
      new%underline = underline
      new%blink = blink
      new%blink_rapid = blink_rapid
      new%reverse = reverse
      new%hidden = hidden
      new%crossed = crossed

      new%black = black
      new%red = red
      new%green = green
      new%yellow = yellow
      new%blue = blue
      new%magenta = magenta
      new%cyan = cyan
      new%white = white
      new%gray  = gray
      new%bright_red  = bright_red
      new%bright_green  = bright_green
      new%bright_yellow  = bright_yellow
      new%bright_blue  = bright_blue
      new%bright_magenta  = bright_magenta
      new%bright_cyan  = bright_cyan
      new%bright_white  = bright_white

      new%bg_black = bg_black
      new%bg_red = bg_red
      new%bg_green = bg_green
      new%bg_yellow = bg_yellow
      new%bg_blue = bg_blue
      new%bg_magenta = bg_magenta
      new%bg_cyan = bg_cyan
      new%bg_white = bg_white
      new%bg_gray = bg_gray
      new%bg_bright_red = bg_bright_red
      new%bg_bright_green = bg_bright_green
      new%bg_bright_yellow = bg_bright_yellow
      new%bg_bright_blue = bg_bright_blue
      new%bg_bright_magenta = bg_bright_magenta
      new%bg_bright_cyan = bg_bright_cyan
      new%bg_bright_white = bg_bright_white
   end if
end function new_terminal

!> Add two escape sequences, attributes in the right value override the left value ones.
pure function add(lval, rval) result(code)
   !> First escape code
   type(ansi_code), intent(in) :: lval
   !> Second escape code
   type(ansi_code), intent(in) :: rval
   !> Combined escape code
   type(ansi_code) :: code

   code%style = merge(rval%style, lval%style, rval%style >= 0)
   code%fg = merge(rval%fg, lval%fg, rval%fg >= 0)
   code%bg = merge(rval%bg, lval%bg, rval%bg >= 0)
end function add


!> Concatenate an escape code with a string and turn it into an actual escape sequence
pure function concat_left(lval, code) result(str)
   !> String to add the escape code to
   character(len=*), intent(in) :: lval
   !> Escape sequence
   type(ansi_code), intent(in) :: code
   !> Concatenated string
   character(len=:), allocatable :: str

   str = lval // escape(code)
end function concat_left

!> Concatenate an escape code with a string and turn it into an actual escape sequence
pure function concat_right(code, rval) result(str)
   !> String to add the escape code to
   character(len=*), intent(in) :: rval
   !> Escape sequence
   type(ansi_code), intent(in) :: code
   !> Concatenated string
   character(len=:), allocatable :: str

   str = escape(code) // rval
end function concat_right


!> Transform a color code into an actual ANSI escape sequence
pure function escape(code) result(str)
   !> Color code to be used
   type(ansi_code), intent(in) :: code
   !> ANSI escape sequence representing the color code
   character(len=:), allocatable :: str

   if (anycolor(code)) then
      str = achar(27) // "[0"  ! Always reset the style
      if (code%style > 0) str = str // ";" // to_string(code%style)
      if (code%fg >= 0) str = str // ";" // to_string(code%fg)
      if (code%bg >= 0) str = str // ";" // to_string(code%bg)
      str = str // "m"
   else
      str = ""
   end if
end function escape

!> Check whether the code describes any color or is just a stub
pure function anycolor(code)
   !> Escape sequence
   type(ansi_code), intent(in) :: code
   !> Any color / style is active
   logical :: anycolor

   anycolor = code%fg >= 0 .or. code%bg >= 0 .or. code%style >= 0
end function anycolor

end module tomlf_terminal
