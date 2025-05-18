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

!> Utilities for handling input and output operations
module tomlf_utils_io
   use tomlf_constants, only : tfc
   implicit none
   private

   public :: read_whole_file, read_whole_line


contains

!> Read a whole file into an array of characters
subroutine read_whole_file(filename, string, stat)
   !> File to read
   character(*, tfc), intent(in) :: filename
   !> Array of characters representing the file
   character(:, tfc), allocatable, intent(out) :: string
   !> Error status
   integer, intent(out) :: stat

   integer :: io, length

   open(file=filename, &
      & status="old", &
      & access="stream", & 
      & position="append", &
      & newunit=io, &
      & iostat=stat)
   if (stat == 0) then
      inquire(unit=io, pos=length)
      allocate(character(length-1, tfc) :: string, stat=stat)
   end if
   if (stat == 0) then
      read(io, pos=1, iostat=stat) string(:length-1)
   end if
   if (stat == 0) then
      close(io)
   end if
end subroutine read_whole_file

!> Read a whole line from a formatted unit into a deferred length character variable
subroutine read_whole_line(io, string, stat)
   !> Formatted IO unit
   integer, intent(in) :: io
   !> Line to read
   character(:, tfc), allocatable, intent(out) :: string
   !> Status of operation
   integer, intent(out) :: stat

   integer, parameter :: bufsize = 4096
   character(bufsize, tfc) :: buffer, msg
   integer :: chunk
   logical :: opened

   if (io /= -1) then
      inquire(unit=io, opened=opened)
   else
      opened = .false.
   end if

   if (opened) then
      open(unit=io, pad="yes", iostat=stat)
   else
      stat = 1
      msg = "Unit is not connected"
   end if

   string = ""
   do while (stat == 0)
      read(io, '(a)', advance='no', iostat=stat, size=chunk) buffer
      if (stat > 0) exit
      string = string // buffer(:chunk)
   end do
   if (is_iostat_eor(stat)) stat = 0
end subroutine read_whole_line

end module tomlf_utils_io
