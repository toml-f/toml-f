! This file is part of toml-f.
!
! Copyright (C) 2019-2020 Sebastian Ehlert
!
! toml-f is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! toml-f is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with toml-f.  If not, see <https://www.gnu.org/licenses/>.

module tomlf08
   use tomlf08_constants
   use tomlf08_type
   use tomlf08_utils
   use tomlf08_ser
   use tomlf08_de
   implicit none

contains

subroutine tomlf08_version(vstr)
   character(len=:), allocatable, intent(out) :: vstr
   vstr = "0.1"
end subroutine tomlf08_version

end module tomlf08
