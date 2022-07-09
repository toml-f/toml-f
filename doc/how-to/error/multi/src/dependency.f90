!> Demo module for fpm-like dependency reading
module demo_dependency
  use tomlf, only : toml_table, toml_context, toml_key, get_value
  implicit none

  !> Dummy dependency type storing only the name of the dependency
  type :: dependency_type
    !> The name of the dependency
    character(:), allocatable :: name
  end type dependency_type

contains

  !> Load a list of dependencies from a table
  subroutine load_dependencies(dependencies, table, context)
    !> List of dependencies
    type(dependency_type), allocatable, intent(out) :: dependencies(:)
    !> TOML data structure
    type(toml_table), intent(inout) :: table
    !> Context for error messages
    type(toml_context), intent(in) :: context

    integer :: it, origin
    type(toml_key), allocatable :: list(:)
    type(toml_table), pointer :: child

    call table%get_keys(list)
    allocate(dependencies(size(list)))

    do it = 1, size(list)
      call get_value(table, list(it), child, origin=origin)
      if (.not.associated(child)) then
        print '(a)', context%report("All entries must be subtables", &
          & origin, table%origin, "must be a subtable", "required for this table")
        stop 1
      end if
      call load_dependency(dependencies(it), list(it), child, context)
    end do
  end subroutine load_dependencies

  !> Load a single dependency from a table
  subroutine load_dependency(dependency, name, table, context)
    !> Information about the dependency
    type(dependency_type), intent(out) :: dependency
    !> Name of the dependency
    type(toml_key), intent(in) :: name
    !> TOML data structure
    type(toml_table), intent(inout) :: table
    !> Context for error messages
    type(toml_context), intent(in) :: context

    integer :: git_origin, path_origin
    character(:), allocatable :: git, path
    type(toml_key), allocatable :: list(:)

    dependency%name = name%key

    call table%get_keys(list)

    call get_value(table, "git", git, origin=git_origin)
    call get_value(table, "path", path, origin=path_origin)

    if (allocated(git) .and. allocated(path)) then
      if (git_origin < path_origin) then
        print '(a)', context%report("Remote dependency cannot have local path", &
          & path_origin, git_origin, &
          & "cannot have local path", "remote dependency already defined")
      else
        print '(a)', context%report("Local dependency cannot have remote repository", &
          & git_origin, path_origin, &
          & "cannot have remote repository", "local dependency already defined")
      end if
      stop 1
    end if
  end subroutine load_dependency

end module demo_dependency
