!> Input model for a computational chemistry simulation program
module demo_input
  use demo_error, only : error_type
  use tomlf, only : toml_table, toml_stat, get_value
  implicit none
  private

  public :: simulation_input

  !> Input for the Slater-Koster integral tables
  type :: skf_input
    !> Format string to find Slater-Koster files
    character(len=:), allocatable :: format_string
    ! ... more settings for the Slater-Koster input
  end type skf_input

  !> Input for the self-consistent charge iterations
  type :: scc_input
    !> Convergence tolerance for energy minimization
    real :: tolerance
    ! ... more settings for the SCC input
  end type scc_input

  !> Input for DFTB Hamiltonian
  type :: dftb_input
    !> Self-consistent field specific settings
    type(scc_input), allocatable :: scc
    !> Slater-Koster table specific settings
    type(skf_input) :: skf
    ! ... more settings for the DFTB input
  end type dftb_input

  !> Input for the Hamiltonian used in the simulation
  type :: hamiltonian_input
    !> DFTB Hamiltonian specific settings
    type(dftb_input), allocatable :: dftb
    ! ... more settings for the Hamiltonian input
  end type hamiltonian_input

  !> Input for analysis of Hamiltonian
  type :: analysis_input
    !> Evaluate derivatives of energy expression
    logical :: calculate_forces
    ! ... more settings for the analysis input
  end type analysis_input

  !> Input for complete simulation
  type :: simulation_input
    !> Hamiltonian used for simulation, always needed
    type(hamiltonian_input) :: hamiltonian
    !> Analysis to run after successful Hamiltonian evaluation
    type(analysis_input), allocatable :: analysis
    ! ... more settings for the simulation input
  end type simulation_input

contains

  !> Read root document
  subroutine read_simulation(error, input, table)
    !> Error handler
    type(error_type), allocatable :: error
    !> Simulation input to be read
    type(simulation_input), intent(out) :: input
    !> Data structure
    type(toml_table), intent(inout) :: table

    type(toml_table), pointer :: child
    integer :: stat

    call get_value(table, "hamiltonian", child)
    if (.not.associated(child)) then
      call fatal_error(error, "No hamiltonian section found in input file")
      return
    end if
    call read_hamiltonian(error, input%hamiltonian, child)
    if (allocated(error)) return

    call get_value(table, "analysis", child, requested=.false., stat=stat)
    if (stat /= toml_stat%success) then
      call fatal_error(error, "No analysis section found in input file")
      return
    end if
    if (associated(child)) then
      allocate(input%analysis)
      call read_analysis(error, input%analysis, child)
      if (allocated(error)) return
    end if

    ! ... read further values
  end subroutine read_simulation

  !> Read Hamiltonian from node
  subroutine read_hamiltonian(error, input, table)
    !> Error handler
    type(error_type), allocatable :: error
    !> Hamiltonian input to be read
    type(hamiltonian_input), intent(out) :: input
    !> Data structure
    type(toml_table), intent(inout) :: table

    type(toml_table), pointer :: child

    call get_value(table, "dftb", child, requested=.false.)
    if (associated(child)) then
      allocate(input%dftb)
      call read_dftb(error, input%dftb, child)
      if (allocated(error)) return
    end if

    ! ... read further values
  end subroutine read_hamiltonian

  !> Read DFTB Hamiltonian from node
  subroutine read_dftb(error, input, table)
    !> Error handler
    type(error_type), allocatable :: error
    !> DFTB Hamiltonian input to be read
    type(dftb_input), intent(out) :: input
    !> Data structure
    type(toml_table), intent(inout) :: table

    type(toml_table), pointer :: child

    call get_value(table, "scc", child, requested=.false.)
    if (associated(child)) then
      allocate(input%scc)
      call read_scc(error, input%scc, child)
      if (allocated(error)) return
    end if

    call get_value(table, "skf", child)
    if (.not.associated(child)) then
      call fatal_error(error, "No skf section found in dftb table")
      return
    end if
    call read_skf(error, input%skf, child)
    if (allocated(error)) return

    ! ... read further values
  end subroutine read_dftb

  !> Read SCC from node
  subroutine read_scc(error, input, table)
    !> Error handler
    type(error_type), allocatable :: error
    !> SCC input to be read
    type(scc_input), intent(out) :: input
    !> Data structure
    type(toml_table), intent(inout) :: table

    call get_value(table, "tolerance", input%tolerance, 1.0e-6)
    ! ... read further values
  end subroutine read_scc

  !> Read Slater-Koster files from node
  subroutine read_skf(error, input, table)
    !> Error handler
    type(error_type), allocatable :: error
    !> Slater-Koster input to be read
    type(skf_input), intent(out) :: input
    !> Data structure
    type(toml_table), intent(inout) :: table

    call get_value(table, "format-string", input%format_string, "{}-{}.skf")
    ! ... read further values
  end subroutine read_skf

  !> Read analysis from node
  subroutine read_analysis(error, input, table)
    !> Error handler
    type(error_type), allocatable :: error
    !> Analysis input to be read
    type(analysis_input), intent(out) :: input
    !> Data structure
    type(toml_table), intent(inout) :: table

    call get_value(table, "calculate-forces", input%calculate_forces, .false.)
    ! ... read further values
  end subroutine read_analysis

end module demo_input
