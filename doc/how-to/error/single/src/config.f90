!> Module for reading in configuration data
module demo_config
  use tomlf, only : toml_table, toml_context, toml_level, get_value
  implicit none

  !> Configuration data
  type :: config_type
    !> Time step for simulation
    real :: timestep
  end type config_type

  !> Threshold for warning on large time step
  real, parameter :: large_timestep = 10.0

contains

  !> Load configuration data from TOML data structure
  subroutine load_config(config, table, context)
    !> Instance of the configuration data
    type(config_type), intent(out) :: config
    !> TOML data structure
    type(toml_table), intent(inout) :: table
    !> Context for reporting errors
    type(toml_context), intent(in) :: context

    integer :: stat, origin

    call get_value(table, "timestep", config%timestep, 0.5, stat=stat, origin=origin)
    if (stat /= 0) then
      print '(a)', context%report("Cannot read timestep", &
        & origin, "expected real value")
      stop 1
    end if

    if (config%timestep <= 0) then
      print '(a)', context%report("Timestep must be positive", &
        & origin, "expected positive value")
      stop 1
    end if

    if (config%timestep > large_timestep) then
      print '(a)', context%report("Large timesteps can lead to instable simulations", &
        & origin, level=toml_level%warning)
    end if

  end subroutine load_config

end module demo_config
