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

module tomlf_de_context
   use tomlf_de_token, only : toml_token, resize
   implicit none
   private

   public :: toml_context


   type :: toml_context
      type(toml_token), allocatable :: token(:)
      integer :: top = 0
   contains
      procedure :: push_back
   end type toml_context

contains

subroutine push_back(self, token)
   class(toml_context), intent(inout) :: self
   type(toml_token), intent(in) :: token

   if (.not.allocated(self%token)) call resize(self%token)
   if (self%top >= size(self%token)) call resize(self%token)

   self%top = self%top + 1
   self%token(self%top) = token
end subroutine push_back


end module tomlf_de_context
