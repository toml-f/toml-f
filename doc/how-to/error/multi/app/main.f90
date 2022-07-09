program demo
  use tomlf, only : toml_table, toml_error, toml_context, toml_load, get_value
  use demo_dependency, only : dependency_type, load_dependencies
  implicit none

  type(dependency_type), allocatable :: deps(:)
  character(128) :: arg

  call get_command_argument(1, arg)

  block
    type(toml_table), allocatable :: table
    type(toml_error), allocatable :: error
    type(toml_context) :: context

    type(toml_table), pointer :: child

    call toml_load(table, trim(arg), context=context, error=error)
    if (allocated(error)) then
      print '(a)', error%message
      stop 1
    end if

    call get_value(table, "dependencies", child)
    if (associated(child)) then
      call load_dependencies(deps, child, context)
    end if
  end block
end program demo
