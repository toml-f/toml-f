module complex_reader
  use tomlf, only : toml_table, toml_array, get_value, len
  implicit none
  private

  public :: read_complex_config

contains

  subroutine read_complex_config(table)
    type(toml_table), intent(inout) :: table

    type(toml_table), pointer :: grid, solver, output, bc, bc_child
    type(toml_array), pointer :: fields
    integer :: nx, ny, nz, max_iter, frequency, i
    real :: tolerance
    character(len=:), allocatable :: method, format, field_name
    real, allocatable :: inlet_velocity(:)

    ! Read grid parameters
    call get_value(table, "grid", grid)
    call get_value(grid, "nx", nx)
    call get_value(grid, "ny", ny)
    call get_value(grid, "nz", nz)
    print '(a, 3(i0, 1x))', "Grid: ", nx, ny, nz

    ! Read solver parameters
    call get_value(table, "solver", solver)
    call get_value(solver, "max_iter", max_iter)
    call get_value(solver, "tolerance", tolerance)
    call get_value(solver, "method", method)
    print '(a, i0)', "Max iterations: ", max_iter
    print '(a, es12.5)', "Tolerance: ", tolerance
    print '(a)', "Method: " // method

    ! Read output parameters
    call get_value(table, "output", output)
    call get_value(output, "frequency", frequency)
    call get_value(output, "format", format)
    print '(a, i0)', "Output frequency: ", frequency
    print '(a)', "Output format: " // format

    ! Read output fields array
    call get_value(output, "fields", fields)
    print '(a)', "Output fields:"
    do i = 1, len(fields)
      call get_value(fields, i, field_name)
      print '(a, i0, a)', "  ", i, ": " // field_name
    end do

    ! Read boundary conditions (nested tables)
    call get_value(table, "boundary_conditions", bc)
    
    ! Read inlet BC
    call get_value(bc, "inlet", bc_child)
    call get_value(bc_child, "value", fields)
    allocate(inlet_velocity(len(fields)))
    do i = 1, size(inlet_velocity)
      call get_value(fields, i, inlet_velocity(i))
    end do
    print '(a, 3(f8.4, 1x))', "Inlet velocity: ", inlet_velocity

  end subroutine read_complex_config

end module complex_reader
