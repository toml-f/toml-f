!> Configuration data for the manifest linting
module fpm_lint_config
  use tomlf, only : toml_table, toml_context, toml_terminal, toml_error, &
    & toml_stat, get_value
  implicit none

  !> Configuration for the manifest linting
  type :: lint_config
    !> Check package name
    logical :: package_name
    !> Check all key paths
    logical :: bare_keys
  end type lint_config

contains

  !> Load the configuration for the linter from the package manifest
  subroutine load_lint_config(config, table, context, terminal, error)
    !> Configuration for the linter
    type(lint_config), intent(out) :: config
    !> TOML data structure representing the manifest
    type(toml_table), intent(inout) :: table
    !> Context describing the data structure
    type(toml_context), intent(in) :: context
    !> Terminal for output
    type(toml_terminal), intent(in) :: terminal
    !> Error handler
    type(toml_error), allocatable, intent(out) :: error

    integer :: origin, stat
    type(toml_table), pointer :: child1, child2, child3

    call get_value(table, "extra", child1, origin=origin)
    if (.not.associated(child1)) then
      call make_error(error, context%report("The 'extra' table is missing.", &
        & origin, "expected table", color=terminal))
      return
    end if
    call get_value(child1, "fpm", child2, origin=origin)
    if (.not.associated(child2)) then
      call make_error(error, context%report("The 'fpm' table is missing.", &
        & origin, "expected table", color=terminal))
      return
    end if
    call get_value(child2, "lint", child3, origin=origin)
    if (.not.associated(child3)) then
      call make_error(error, context%report("The 'lint' table is missing.", &
        & origin, "expected table", color=terminal))
      return
    end if

    call get_value(child3, "package-name", config%package_name, .true., &
      & stat=stat, origin=origin)
    if (stat /= toml_stat%success) then
      call make_error(error, context%report("Entry in 'package-name' must be boolean", &
        & origin, "expected boolean value", color=terminal))
      return
    end if
    call get_value(child3, "bare-keys", config%bare_keys, .true., &
      & stat=stat, origin=origin)
    if (stat /= toml_stat%success) then
      call make_error(error, context%report("Entry in 'bare-key' must be boolean", &
        & origin, "expected boolean value", color=terminal))
      return
    end if
  end subroutine load_lint_config

  !> Create an error message
  subroutine make_error(error, message)
    !> Error handler
    type(toml_error), allocatable, intent(out) :: error
    !> Message to be displayed
    character(len=*), intent(in) :: message

    allocate(error)
    error%message = message
    error%stat = toml_stat%fatal
  end subroutine make_error

end module fpm_lint_config
