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

!> Merge TOML data structures, the merge policy can be adjusted.
!>
!> Note that the context information cannot be preserved.
module tomlf_build_merge
   use tomlf_constants, only : tfc
   use tomlf_type, only : toml_table, toml_array, toml_keyval, toml_value, &
      & toml_key, cast_to_keyval, len
   implicit none
   private

   public :: merge_table, merge_array, merge_policy, toml_merge_config


   !> Possible merge policies
   type :: enum_policy

      !> Overwrite existing values
      integer :: overwrite = 1

      !> Preserve existing values
      integer :: preserve = 2

      !> Append to existing values
      integer :: append = 3
   end type enum_policy

   !> Actual enumerator for merging data structures
   type(enum_policy), parameter :: merge_policy = enum_policy()


   !> Configuration for merging data structures
   type :: toml_merge_config

      !> Policy for merging tables
      integer :: table = merge_policy%append

      !> Policy for merging arrays
      integer :: array = merge_policy%preserve

      !> Policy for merging values
      integer :: keyval = merge_policy%preserve
   end type toml_merge_config

   !> Constructor for merge configuration
   interface toml_merge_config
      module procedure :: new_merge_config
   end interface toml_merge_config


contains


!> Create a new merge configuration
pure function new_merge_config(table, array, keyval) result(config)

   !> Policy for merging tables
   character(*), intent(in), optional :: table

   !> Policy for merging arrays
   character(*), intent(in), optional :: array

   !> Policy for merging values
   character(*), intent(in), optional :: keyval

   !> Merge policy
   type(toml_merge_config) :: config

   if (present(table)) call set_enum(config%table, table)
   if (present(array)) call set_enum(config%array, array)
   if (present(keyval)) call set_enum(config%keyval, keyval)

contains

   pure subroutine set_enum(enum, str)
      character(*), intent(in) :: str
      integer, intent(inout) :: enum

      select case(str)
      case("append")
         enum = merge_policy%append
      case("overwrite")
         enum = merge_policy%overwrite
      case("preserve")
         enum = merge_policy%preserve
      end select
   end subroutine set_enum

end function new_merge_config


!> Merge TOML tables by appending their values
recursive subroutine merge_table(lhs, rhs, config)

   !> Instance of table to merge into
   class(toml_table), intent(inout) :: lhs

   !> Instance of table to be merged
   class(toml_table), intent(inout) :: rhs

   !> Merge policy
   type(toml_merge_config), intent(in), optional :: config

   type(toml_merge_config) :: policy
   type(toml_key), allocatable :: list(:)
   class(toml_value), pointer :: ptr1, ptr2
   class(toml_keyval), pointer :: kv
   class(toml_value), allocatable :: tmp
   logical :: has_key
   integer :: i, n, stat

   policy = toml_merge_config()
   if (present(config)) policy = config

   call rhs%get_keys(list)
   n = size(list, 1)

   do i = 1, n
      if (allocated(tmp)) deallocate(tmp)
      call rhs%get(list(i)%key, ptr1)
      has_key = lhs%has_key(list(i)%key)
      select type(ptr1)
      class is(toml_keyval)
         if (has_key .and. policy%keyval == merge_policy%overwrite) then
            call lhs%delete(list(i)%key)
            has_key = .false.
         end if
         if (.not.has_key) then
            allocate(tmp, source=ptr1)
            kv => cast_to_keyval(tmp)
            kv%origin_value = 0
            kv%origin = 0
            call lhs%push_back(tmp, stat)
         end if

      class is(toml_array)
         if (has_key .and. policy%array == merge_policy%overwrite) then
            call lhs%delete(list(i)%key)
            has_key = .false.
         end if
         if (has_key .and. policy%array == merge_policy%append) then
            call lhs%get(list(i)%key, ptr2)
            select type(ptr2)
            class is(toml_array)
               call merge_array(ptr2, ptr1)
            end select
         end if
         if (.not.has_key) then
            allocate(tmp, source=ptr1)
            tmp%origin = 0
            call lhs%push_back(tmp, stat)
         end if

      class is(toml_table)
         if (has_key .and. policy%table == merge_policy%overwrite) then
            call lhs%delete(list(i)%key)
            has_key = .false.
         end if
         if (has_key .and. policy%table == merge_policy%append) then
            call lhs%get(list(i)%key, ptr2)
            select type(ptr2)
            class is(toml_table)
               call merge_table(ptr2, ptr1, policy)
            end select
         end if
         if (.not.has_key) then
            allocate(tmp, source=ptr1)
            tmp%origin = 0
            call lhs%push_back(tmp, stat)
         end if
      end select
   end do

end subroutine merge_table


!> Append values from one TOML array to another
recursive subroutine merge_array(lhs, rhs)

   !> Instance of array to merge into
   class(toml_array), intent(inout) :: lhs

   !> Instance of array to be merged
   class(toml_array), intent(inout) :: rhs

   class(toml_value), pointer :: ptr
   class(toml_value), allocatable :: tmp
   integer :: n, i, stat

   n = len(rhs)

   do i = 1, n
      call rhs%get(i, ptr)
      if (allocated(tmp)) deallocate(tmp)
      allocate(tmp, source=ptr)
      call lhs%push_back(tmp, stat)
   end do

end subroutine merge_array


end module tomlf_build_merge
