program use_namelist
  use namelist_reader, only : read_simulation_namelist
  implicit none

  integer :: iterations, nx, ny
  real :: dt, viscosity
  character(len=256) :: mesh_file

  ! Read configuration from namelist
  call read_simulation_namelist(iterations, nx, ny, dt, viscosity, mesh_file)

  ! Display the configuration
  print '(a)', "=== Simulation Configuration (from Namelist) ==="
  print '(a, i0)', "Iterations: ", iterations
  print '(a, i0, a, i0)', "Grid: ", nx, " x ", ny
  print '(a, es12.5)', "Time step: ", dt
  print '(a, es12.5)', "Viscosity: ", viscosity
  print '(a)', "Mesh file: " // trim(mesh_file)

end program use_namelist
