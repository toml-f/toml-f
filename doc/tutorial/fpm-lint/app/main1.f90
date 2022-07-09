program main
  use, intrinsic :: iso_fortran_env, only : stderr => error_unit, stdout => output_unit
  use fpm_lint_config, only : lint_config, load_lint_config
  use fpm_lint_utils, only : get_argument
  use tomlf, only : toml_table, toml_load, toml_error, toml_context, toml_parser_config, &
    & toml_terminal
  implicit none
  logical, parameter :: color = .true.
  character(:), allocatable :: manifest
  type(toml_terminal) :: terminal
  type(toml_table), allocatable :: table
  type(toml_error), allocatable :: error
  type(toml_context) :: context
  type(lint_config) :: config

  terminal = toml_terminal(color)
  call get_argument(1, manifest)
  if (.not.allocated(manifest)) manifest = "fpm.toml"

  call toml_load(table, manifest, error=error, context=context, &
    & config=toml_parser_config(color=terminal))
  call handle_error(error)

  call load_lint_config(config, table, context, terminal, error)
  call handle_error(error)

contains

  subroutine handle_error(error)
    type(toml_error), intent(in), optional :: error
    if (present(error)) then
      write(stderr, '(a)') error%message
      stop 1
    end if
  end subroutine handle_error

end program main
