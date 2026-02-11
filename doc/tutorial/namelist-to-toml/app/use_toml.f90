program use_toml
  use toml_reader, only : read_simulation_toml
  use tomlf, only : toml_table, toml_parse, toml_error
  implicit none

  type(toml_table), allocatable :: table
  type(toml_error), allocatable :: error
  integer :: iterations, nx, ny, io
  real :: dt, viscosity
  character(len=:), allocatable :: mesh_file

  ! Parse TOML file
  open(file="simulation.toml", newunit=io, status="old")
  call toml_parse(table, io, error)
  close(io)

  if (allocated(error)) then
    print '(a)', "Error parsing TOML: " // error%message
    stop 1
  end if

  ! Read configuration from TOML
  call read_simulation_toml(table, iterations, nx, ny, dt, viscosity, mesh_file)

  ! Display the configuration
  print '(a)', "=== Simulation Configuration (from TOML) ==="
  print '(a, i0)', "Iterations: ", iterations
  print '(a, i0, a, i0)', "Grid: ", nx, " x ", ny
  print '(a, es12.5)', "Time step: ", dt
  print '(a, es12.5)', "Viscosity: ", viscosity
  print '(a)', "Mesh file: " // mesh_file

end program use_toml
