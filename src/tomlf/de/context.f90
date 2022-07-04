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

!> Provides a container to store tokens for later use
module tomlf_de_context
   use tomlf_constants, only : tfc
   use tomlf_de_token, only : toml_token, resize
   use tomlf_diagnostic, only : toml_diagnostic, toml_label, render, level_error, level_info
   use tomlf_terminal, only : toml_terminal
   implicit none
   private

   public :: toml_context

   !> Container storing tokens
   type :: toml_context
      !> Filename of the input
      character(:, tfc), allocatable :: filename
      !> Actual source
      character(:, tfc), allocatable :: source
      !> Stack of stored tokens
      type(toml_token), allocatable :: token(:)
      !> Last stored token
      integer :: top = 0
   contains
      !> Push a new token to the stack
      procedure :: push_back
      !> Create a report
      generic :: report => report1, report2
      !> Create a report with a single label
      procedure :: report1
      !> Create a report with a two labels
      procedure :: report2
   end type toml_context

contains

!> Push a new token to the stack
subroutine push_back(self, token)
   !> Instance of the token storage
   class(toml_context), intent(inout) :: self
   !> New token to be added
   type(toml_token), intent(in) :: token

   if (.not.allocated(self%token)) call resize(self%token)
   if (self%top >= size(self%token)) call resize(self%token)

   self%top = self%top + 1
   self%token(self%top) = token
end subroutine push_back

!> Create a report with a single label
pure function report1(self, message, origin, label, color) result(string)
   !> Instance of the token storage
   class(toml_context), intent(in) :: self
   !> Message for the report
   character(*, tfc), intent(in) :: message
   !> Position to report at
   integer, intent(in) :: origin
   !> String for the label
   character(*, tfc), intent(in) :: label
   !> Color terminal
   type(toml_terminal), intent(in) :: color
   !> Final rendered report
   character(:, tfc), allocatable :: string

   type(toml_diagnostic) :: diagnostic
   type(toml_label), allocatable :: labels(:)

   if (origin > 0 .and. origin <= self%top) then
      allocate(labels(1))
      labels(1) = toml_label(level_error, label, &
         &  self%token(origin)%first, self%token(origin)%last, .true.)
   end if

   diagnostic = toml_diagnostic( &
      & level_error, &
      & message, &
      & self%filename, &
      & labels)

   string = render(diagnostic, self%source, color)
end function report1

!> Create a report with two labels
pure function report2(self, message, origin1, origin2, label1, label2, color) result(string)
   !> Instance of the token storage
   class(toml_context), intent(in) :: self
   !> Message for the report
   character(*, tfc), intent(in) :: message
   !> Position to report at
   integer, intent(in) :: origin1, origin2
   !> String for the label
   character(*, tfc), intent(in) :: label1, label2
   !> Color terminal
   type(toml_terminal), intent(in) :: color
   !> Final rendered report
   character(:, tfc), allocatable :: string

   type(toml_diagnostic) :: diagnostic
   type(toml_label), allocatable :: labels(:)

   if (origin1 > 0 .and. origin1 <= self%top &
      & .and. origin2 > 0 .and. origin2 <= self%top) then
      allocate(labels(2))
      labels(1) = toml_label(level_error, label1, &
         &  self%token(origin1)%first, self%token(origin1)%last, .true.)
      labels(2) = toml_label(level_info, label2, &
         &  self%token(origin1)%first, self%token(origin1)%last, .false.)
   end if

   diagnostic = toml_diagnostic( &
      & level_error, &
      & message, &
      & self%filename, &
      & labels)

   string = render(diagnostic, self%source, color)
end function report2

end module tomlf_de_context
