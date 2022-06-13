!> Defines a simple dependency type to express requirements of a project.
module demo_requirements
  use demo_error, only : error_type, fatal_error
  use tomlf, only : toml_table, toml_key, get_value, set_value
  implicit none
  private

  public :: requirement_type, new_requirement, new_requirements

  !> Declaration of a dependency
  type :: requirement_type
    !> Name to identify dependency
    character(len=:), allocatable :: name
    ! ... further declaration of entries
  end type requirement_type

contains

  !> Create a new list of requirements from a table
  subroutine new_requirements(error, table, req)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    !> TOML data structure
    type(toml_table), intent(inout) :: table
    !> List of all dependencies
    type(requirement_type), allocatable, intent(out) :: req(:)

    integer :: ikey
    type(toml_key), allocatable :: list(:)
    type(toml_table), pointer :: child
    type(toml_table), allocatable, target :: dummy
    character(len=:), allocatable :: version

    ! Get all entries of current table, the `list` is guaranteed to be allocated
    call table%get_keys(list)
    allocate(req(size(list)))

    do ikey = 1, size(list)
      call get_value(table, list(ikey), child)

      ! Not providing a subtable is okay if a string provides the version constraint
      if (.not. associated(child)) then
        call get_value(table, list(ikey), version)
        if (.not.allocated(version)) then
          call fatal_error(error, "Requirement '"//list(ikey)%key//&
            & "' must be version constraint or subtable")
          exit
        end if

        ! Create a dummy table we can reference when initializing
        dummy = toml_table()
        call set_value(dummy, "version", version)
        child => dummy
      end if

      ! Initialize dependency from subtable
      call new_requirement(error, child, req(ikey), list(ikey)%key)

      ! Cleanup, alternatively the dummy could be pushed back into the main table
      if (allocated(dummy)) deallocate(dummy)
      ! Leave loop in case of error
      if (allocated(error)) exit
    end do
    ! Requirements are in a half-finished state, invalidate them to avoid confusion
    if (allocated(error)) deallocate(req)
  end subroutine new_requirements

  !> Initialize a dependency from a TOML data structure
  subroutine new_requirement(error, table, req, name)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    !> TOML data structure
    type(toml_table), intent(inout) :: table
    !> New dependency object
    type(requirement_type), intent(out) :: req
    !> Name of the dependency
    character(len=*), intent(in) :: name

    req%name = name
    ! ... further reading of entries
  end subroutine new_requirement

end module demo_requirements
