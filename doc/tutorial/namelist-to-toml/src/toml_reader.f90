module toml_reader
  use tomlf, only : toml_table, toml_error, get_value
  implicit none
  private

  public :: read_simulation_toml

contains

  subroutine read_simulation_toml(table, iterations, nx, ny, dt, viscosity, mesh_file)
    type(toml_table), intent(inout) :: table
    integer, intent(out) :: iterations, nx, ny
    real, intent(out) :: dt, viscosity
    character(len=:), allocatable, intent(out) :: mesh_file

    type(toml_table), pointer :: child

    ! Get simulation parameters with defaults
    call get_value(table, "simulation", child)
    call get_value(child, "iterations", iterations, 100)
    call get_value(child, "dt", dt, 0.01)
    call get_value(child, "viscosity", viscosity, 1.0e-3)

    ! Get grid parameters with defaults
    call get_value(table, "grid", child)
    call get_value(child, "nx", nx, 64)
    call get_value(child, "ny", ny, 64)

    ! Get file paths with defaults
    call get_value(table, "files", child)
    call get_value(child, "mesh", mesh_file, "mesh.h5")

  end subroutine read_simulation_toml

end module toml_reader
