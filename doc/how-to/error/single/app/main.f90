program demo
  use tomlf, only : toml_table, toml_error, toml_context, toml_load
  use demo_config, only : config_type, load_config
  implicit none

  type(config_type) :: config
  character(128) :: arg

  call get_command_argument(1, arg)

  block
    type(toml_table), allocatable :: table
    type(toml_error), allocatable :: error
    type(toml_context) :: context

    call toml_load(table, trim(arg), context=context, error=error)
    if (allocated(error)) then
      print '(a)', error%message
      stop 1
    end if

    call load_config(config, table, context)
  end block
end program demo
