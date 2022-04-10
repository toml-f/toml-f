module demo_serde
  use serde_class, only : serde_record
  use serde_error, only : error_type, fatal_error
  use tomlf, only : toml_table, get_value, set_value
  implicit none
  private

  public :: example_record

  type, extends(serde_record) :: example_record
    integer :: nrun
    real :: alpha
    character(len=:), allocatable :: label
  contains
    !> Read configuration data from TOML data structure
    procedure :: load_from_toml
    !> Write configuration data to TOML data structure
    procedure :: dump_to_toml
  end type example_record

contains

  !> Read configuration data from TOML data structure
  subroutine load_from_toml(self, table, error)
    !> Instance of the configuration data
    class(example_record), intent(inout) :: self
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: stat

    call get_value(table, "nrun", self%nrun, 10, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Invalid entry for number of runs")
      return
    end if

    call get_value(table, "alpha", self%alpha, 1.0, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Invalid entry for alpha parameter")
      return
    end if

    call get_value(table, "label", self%label, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Invalid entry for data label")
      return
    end if
  end subroutine load_from_toml


  !> Write configuration data to TOML datastructure
  subroutine dump_to_toml(self, table, error)
    !> Instance of the configuration data
    class(example_record), intent(in) :: self
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call set_value(table, "nrun", self%nrun)
    call set_value(table, "alpha", self%alpha)
    if (allocated(self%label)) then
      call set_value(table, "label", self%label)
    end if
  end subroutine dump_to_toml

end module demo_serde
