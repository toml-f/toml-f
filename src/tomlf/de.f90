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

module tomlf_de
   use tomlf_constants, only : TOML_NEWLINE
   use tomlf_de_character, only : toml_character_tokenizer, new
   use tomlf_error, only : toml_error, io_error
   use tomlf_type, only : toml_table
   implicit none
   private

   public :: toml_parse


   interface toml_parse
      module procedure :: toml_parse_unit
      module procedure :: toml_parse_string
   end interface toml_parse


contains


!> Parse a TOML input from a given IO unit.
subroutine toml_parse_unit(table, unit, error)
   use iso_fortran_env
   use tomlf_constants, only: TOML_NEWLINE
   type(toml_table), allocatable, intent(out) :: table
   integer, intent(in) :: unit
   type(toml_error), allocatable, intent(out), optional :: error
   character(len=:), allocatable :: conf
   integer, parameter :: bufsize = 512
   character(len=bufsize) :: buffer
   character(len=bufsize) :: error_msg
   integer :: size
   integer :: stat
   allocate(character(len=0) :: conf)
   do 
      read(unit, '(a)', advance='no', iostat=stat, iomsg=error_msg, size=size) &
         & buffer
      if (stat > 0) exit
      conf = conf // buffer(:size)
      if (stat < 0) then
         if (is_iostat_eor(stat)) then
            stat = 0
            conf = conf // TOML_NEWLINE
         end if
         if (is_iostat_end(stat)) then
            stat = 0
            exit
         end if
      end if
   end do

   if (stat /= 0) then
      if (present(error)) then
         call io_error(error, trim(error_msg))
      else
         write(error_unit, '(a, /, a)') "IO runtime error", trim(error_msg)
      end if
      return
   end if

   call toml_parse_string(table, conf, error)

end subroutine toml_parse_unit


!> Wrapper to parse a TOML string.
subroutine toml_parse_string(table, conf, error)
   use iso_fortran_env, only: error_unit
   type(toml_table), allocatable, intent(out) :: table
   character(len=*), intent(in), target :: conf
   type(toml_error), allocatable, intent(out), optional :: error
   type(toml_character_tokenizer) :: de

   !> connect deserializer to configuration
   call new(de, conf)

   call de%parse

   if (allocated(de%error)) then
      if (present(error)) then
         call move_alloc(de%error, error)
      else
         write(error_unit, '(a)') de%error%message
      end if
      return
   end if

   call move_alloc(de%root, table)

end subroutine toml_parse_string


end module tomlf_de
