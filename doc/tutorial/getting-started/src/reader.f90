module reader
  use tomlf, only : toml_table, toml_array, get_value, len
  implicit none
  private

  public :: read_data

contains

  subroutine read_data(table, title, spectrum)
    type(toml_table), intent(inout) :: table
    character(len=:), allocatable, intent(out) :: title
    real, allocatable, intent(out) :: spectrum(:)

    type(toml_table), pointer :: child
    type(toml_array), pointer :: array
    logical :: reverse
    integer :: ival

    ! Get character value from entry "title"
    call get_value(table, "title", title)

    ! Get subtable reference from entry "spectrum"
    call get_value(table, "spectrum", child)

    ! Get array reference from entry "data"
    call get_value(child, "data", array)

    ! Read all values from the data array
    allocate(spectrum(len(array)))
    do ival = 1, size(spectrum)
      call get_value(array, ival, spectrum(ival))
    end do

    ! Data is stored in reverse order
    call get_value(child, "reverse", reverse, .false.)
    if (reverse) spectrum(:) = spectrum(size(spectrum):1:-1)
  end subroutine read_data

end module reader
