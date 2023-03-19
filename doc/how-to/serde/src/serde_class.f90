!> Definition of configuration data with serde properties.
!> Each data record knows how to serialize and deserialize itself.
module serde_class
  use serde_error, only : error_type, fatal_error
  use tomlf, only : toml_table, toml_error, toml_load, toml_dump
  implicit none
  private

  public :: serde_record

  !> Serializable and deserializable configuration data record
  type, abstract :: serde_record
  contains
    !> Reading of configuration data
    generic :: load => load_from_file, load_from_unit, load_from_toml
    !> Read configuration data from file
    procedure, private :: load_from_file
    !> Read configuration data from formatted unit
    procedure, private :: load_from_unit
    !> Read configuration data from TOML data structure
    procedure(load_from_toml), deferred :: load_from_toml
    !> Writing of configuration data
    generic :: dump => dump_to_file, dump_to_unit, dump_to_toml
    !> Write configuration data to file
    procedure, private :: dump_to_file
    !> Write configuration data to formatted unit
    procedure, private :: dump_to_unit
    !> Write configuration data to TOML data structure
    procedure(dump_to_toml), deferred :: dump_to_toml
  end type serde_record

  abstract interface
    !> Read configuration data from TOML data structure
    subroutine load_from_toml(self, table, error)
      import :: serde_record, toml_table, error_type
      !> Instance of the configuration data
      class(serde_record), intent(inout) :: self
      !> Data structure
      type(toml_table), intent(inout) :: table
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
    end subroutine load_from_toml
    !> Write configuration data to TOML datastructure
    subroutine dump_to_toml(self, table, error)
      import :: serde_record, toml_table, error_type
      !> Instance of the configuration data
      class(serde_record), intent(in) :: self
      !> Data structure
      type(toml_table), intent(inout) :: table
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
    end subroutine dump_to_toml
  end interface

contains

  !> Read configuration data from file
  subroutine load_from_file(self, file, error)
    !> Instance of the configuration data
    class(serde_record), intent(inout) :: self
    !> File name
    character(len=*), intent(in) :: file
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    type(toml_error), allocatable :: parse_error
    type(toml_table), allocatable :: table

    call toml_load(table, file, error=parse_error)

    if (allocated(parse_error)) then
      allocate(error)
      call move_alloc(parse_error%message, error%message)
      return
    end if

    call self%load(table, error)
    if (allocated(error)) return
  end subroutine load_from_file

  !> Read configuration data from file
  subroutine load_from_unit(self, unit, error)
    !> Instance of the configuration data
    class(serde_record), intent(inout) :: self
    !> File name
    integer, intent(in) :: unit
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    type(toml_error), allocatable :: parse_error
    type(toml_table), allocatable :: table

    call toml_load(table, unit, error=parse_error)

    if (allocated(parse_error)) then
      allocate(error)
      call move_alloc(parse_error%message, error%message)
      return
    end if

    call self%load(table, error)
    if (allocated(error)) return
  end subroutine load_from_unit

  !> Write configuration data to file
  subroutine dump_to_file(self, file, error)
    !> Instance of the configuration data
    class(serde_record), intent(in) :: self
    !> File name
    character(len=*), intent(in) :: file
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: unit

    open(file=file, newunit=unit)
    call self%dump(unit, error)
    close(unit)
    if (allocated(error)) return

  end subroutine dump_to_file

  !> Write configuration data to file
  subroutine dump_to_unit(self, unit, error)
    !> Instance of the configuration data
    class(serde_record), intent(in) :: self
    !> Formatted unit
    integer, intent(in) :: unit
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    type(toml_table) :: table

    table = toml_table()

    call self%dump(table, error)

    if (.not.allocated(error)) then
      call toml_dump(table, unit, error)
    end if

  end subroutine dump_to_unit

end module serde_class
