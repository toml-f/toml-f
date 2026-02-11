module namelist_reader
  implicit none
  private

  public :: read_simulation_namelist

contains

  subroutine read_simulation_namelist(iterations, nx, ny, dt, viscosity, mesh_file)
    integer, intent(out) :: iterations, nx, ny
    real, intent(out) :: dt, viscosity
    character(len=*), intent(out) :: mesh_file

    ! Namelist definition
    namelist /simulation/ iterations, nx, ny, dt, viscosity, mesh_file

    ! Default values
    iterations = 100
    nx = 64
    ny = 64
    dt = 0.01
    viscosity = 1.0e-3
    mesh_file = 'mesh.h5'

    ! Read from file
    block
      integer :: io, stat
      
      open(newunit=io, file='simulation.nml', status='old', action='read')
      read(io, nml=simulation, iostat=stat)
      close(io)
      
      if (stat /= 0) then
        print '(a)', "Warning: Error reading namelist, using defaults"
      end if
    end block

  end subroutine read_simulation_namelist

end module namelist_reader
