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

!> Implementation of the package meta data storage
module pkg_package
   use pkg_dependency, only : dependency_data, new_dependency
   use pkg_error, only : error_data, fatal_error
   use pkg_source, only : source_data, new_source
   use tomlf
   use tomlf_type, only : len
   implicit none
   private

   public :: package_data, new_package_data


   !> Central package meta data
   type :: package_data

      !> Name of the package
      character(len=:), allocatable :: name

      !> Version of the package
      character(len=:), allocatable :: version

      !> License
      character(len=:), allocatable :: license

      !> Sources to build the package
      type(source_data), allocatable :: sources(:)

      !> Dependencies required by the package
      type(dependency_data), allocatable :: dependencies(:)

   contains

      !> Produce informative printout
      procedure :: info

   end type package_data


contains


!> Initialize package data from a TOML data structure
subroutine new_package_data(self, table, error)

   !> Instance of the package data
   type(package_data), intent(out) :: self

   !> TOML data structure, might get modified in the process
   type(toml_table), intent(inout) :: table

   !> Error status of the operation
   type(error_data), allocatable, intent(out) :: error

   type(toml_table), pointer :: child, node
   type(toml_array), pointer :: children
   type(toml_key), allocatable :: list(:)
   character(len=:), allocatable :: license
   integer :: stat, idep, isrc, ilic, nlic

   call get_value(table, "name", self%name)
   if (.not.allocated(self%name)) then
      call fatal_error(error, "Package name is not provided")
   end if
   call get_value(table, "version", self%version, "0.0.0")
   call get_value(table, "license", children, requested=.false.)
   if (associated(children)) then
      nlic = len(children)
      if (nlic < 1) then
         call fatal_error(error, "License field requires at least one entry or must be omitted")
         return
      end if
      do ilic = 1, nlic
         call get_value(children, ilic, license)
         if (.not.allocated(license)) then
            call fatal_error(error, "Element of license array is not a string value")
            exit
         end if
         if (.not.allocated(self%license)) then
            call move_alloc(license, self%license)
         else
            self%license = self%license // ' or ' // license
         end if
      end do
      if (allocated(error)) return
   else
      call get_value(table, "license", self%license, "unknown")
   end if

   call get_value(table, "dependencies", child, requested=.false.)
   if (associated(child)) then
      call child%get_keys(list)
      allocate(self%dependencies(size(list)))
      do idep = 1, size(list)
         call get_value(child, list(idep)%key, node, stat=stat)
         if (stat /= toml_stat%success) then
            call fatal_error(error, "Dependency "//list(idep)%key//" must be a table entry")
            exit
         end if
         call new_dependency(self%dependencies(idep), node, error)
         if (allocated(error)) exit
      end do
      if (allocated(error)) return
   end if

   call get_value(table, "source", child, requested=.false.)
   if (associated(child)) then
      allocate(self%sources(1))
      call new_source(self%sources(1), child, error)
   else
      call get_value(table, "source", children, requested=.false.)
      if (.not.associated(children)) then
         call fatal_error(error, "Requires at least one source table")
         return
      end if
      if (.not.is_array_of_tables(children)) then
         call fatal_error(error, "Sources must be provided as table or array of tables")
         return
      end if

      allocate(self%sources(len(children)))
      do isrc = 1, len(children)
         call get_value(children, isrc, node, stat=stat)
         if (stat /= toml_stat%success) then
            call fatal_error(error, "Could not retrieve sources from array entry")
            exit
         end if
         call new_source(self%sources(isrc), node, error)
         if (allocated(error)) exit
      end do
   end if
   if (allocated(error)) return

end subroutine new_package_data


!> Write information on the package data
subroutine info(self, unit)

   !> Instance of the package data
   class(package_data), intent(in) :: self

   !> Unit for IO
   integer, intent(in) :: unit

   integer :: idep, isrc

   write(unit, '(a, t30, a)') "Package name", self%name
   write(unit, '(a, t30, a)') "Package version", self%version
   write(unit, '(a, t30, a)') "Package license", self%license

   write(unit, '(a, t30, i0)') "Sources collected", size(self%sources)
   if (allocated(self%dependencies)) then
      write(unit, '(a, t30, i0)') "Dependencies collected", size(self%dependencies)
   end if

   write(unit, '(a)') "Source targets:"
   do isrc = 1, size(self%sources)
      call self%sources(isrc)%info(unit)
   end do

   if (allocated(self%dependencies)) then
      write(unit, '(a)') "Dependencies:"
      do idep = 1, size(self%dependencies)
         call self%dependencies(idep)%info(unit)
      end do
   end if

end subroutine info


end module pkg_package
