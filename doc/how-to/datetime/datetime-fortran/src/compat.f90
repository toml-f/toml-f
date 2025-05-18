!> Compatibility module to allow working with [[datetime]] objects and TOML data structures
module demo_compat
  use datetime_module, only : datetime
  use tomlf, only : toml_table, toml_array, toml_keyval, toml_key, toml_stat, toml_datetime, &
    & get_value, set_value, add_keyval, len
  implicit none
  private

  public :: get_value, set_value
  public :: assignment(=)

  !> Getter functions to manipulate TOML values
  interface get_value
    module procedure :: get_value_datetime_fortran
    module procedure :: get_child_value_datetime_fortran
    module procedure :: get_key_value_datetime_fortran
    module procedure :: get_elem_value_datetime_fortran
  end interface get_value

  !> Setter functions to manipulate TOML values
  interface set_value
    module procedure :: set_value_datetime_fortran
    module procedure :: set_child_value_datetime_fortran
    module procedure :: set_key_value_datetime_fortran
    module procedure :: set_elem_value_datetime_fortran
  end interface set_value

  !> Define assignment operation between [[toml_datatime]] and [[datetime]] objects
  interface assignment(=)
    module procedure :: convert_to_datetime
    module procedure :: convert_from_datetime
  end interface assignment(=)

contains

  !> Set TOML value to datetime
  subroutine set_value_datetime_fortran(self, val, stat, origin)
    !> Instance of the key-value pair
    class(toml_keyval), intent(inout) :: self
    !> Datetime value
    type(datetime), intent(in) :: val
    !> Status of operation
    integer, intent(out), optional :: stat
    !> Origin in the data structure
    integer, intent(out), optional :: origin

    type(toml_datetime) :: tmp

    tmp = val
    call set_value(self, tmp, stat, origin)
  end subroutine set_value_datetime_fortran

  !> Set TOML value to datetime
  subroutine set_child_value_datetime_fortran(table, key, val, stat, origin)
    !> Instance of the TOML table
    class(toml_table), intent(inout) :: table
    !> Key in this TOML table
    character(*), intent(in) :: key
    !> Datetime value
    type(datetime), intent(in) :: val
    !> Status of operation
    integer, intent(out), optional :: stat
    !> Origin in the data structure
    integer, intent(out), optional :: origin

    type(toml_keyval), pointer :: ptr

    call get_value(table, key, ptr, .true., stat, origin)

    if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
    else
      if (present(stat)) then
        if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
    end if
  end subroutine set_child_value_datetime_fortran

  !> Set TOML value to datetime
  subroutine set_key_value_datetime_fortran(table, key, val, stat, origin)
    !> Instance of the TOML table
    class(toml_table), intent(inout) :: table
    !> Key in this TOML table
    type(toml_key), intent(in) :: key
    !> Datetime value
    type(datetime), intent(in) :: val
    !> Status of operation
    integer, intent(out), optional :: stat
    !> Origin in the data structure
    integer, intent(out), optional :: origin

    call set_value(table, key%key, val, stat, origin)
  end subroutine set_key_value_datetime_fortran

  !> Retrieve TOML value as datetime value
  subroutine set_elem_value_datetime_fortran(array, pos, val, stat, origin)
    !> Instance of the TOML array
    class(toml_array), intent(inout) :: array
    !> Position in the array
    integer, intent(in) :: pos
    !> Datetime value
    type(datetime), intent(in) :: val
    !> Status of operation
    integer, intent(out), optional :: stat
    !> Origin in the data structure
    integer, intent(out), optional :: origin

    type(toml_keyval), pointer :: ptr

    call get_value(array, pos, ptr, stat, origin)

    if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
        call add_keyval(array, ptr, stat)
      end if
    end if

    if (associated(ptr)) then
      call set_value(ptr, val, stat, origin)
    else
      if (present(stat)) stat = toml_stat%fatal
    end if
  end subroutine set_elem_value_datetime_fortran

  !> Retrieve TOML value as datetime
  subroutine get_value_datetime_fortran(self, val, stat, origin)
    !> Instance of the key-value pair
    class(toml_keyval), intent(in) :: self
    !> Datetime value
    type(datetime), intent(out) :: val
    !> Status of operation
    integer, intent(out), optional :: stat
    !> Origin in the data structure
    integer, intent(out), optional :: origin

    type(toml_datetime) :: tmp

    call get_value(self, tmp, stat, origin)
    val = tmp
  end subroutine get_value_datetime_fortran

  !> Retrieve TOML value as datetime
  subroutine get_child_value_datetime_fortran(table, key, val, default, stat, origin)
    !> Instance of the TOML table
    class(toml_table), intent(inout) :: table
    !> Key in this TOML table
    character(*), intent(in) :: key
    !> Datetime value
    type(datetime), intent(out) :: val
    !> Default datetime value
    type(datetime), intent(in), optional :: default
    !> Status of operation
    integer, intent(out), optional :: stat
    !> Origin in the data structure
    integer, intent(out), optional :: origin

    type(toml_keyval), pointer :: ptr

    call get_value(table, key, ptr, present(default), stat, origin)

    if (associated(ptr)) then
      if (allocated(ptr%val)) then
        call get_value(ptr, val, stat, origin)
      else
        if (present(default)) then
          call set_value(ptr, default)
          call get_value(ptr, val, stat=stat)
        else
          if (present(stat)) stat = toml_stat%fatal
        end if
      end if
    end if
  end subroutine get_child_value_datetime_fortran

  !> Retrieve TOML value as datetime
  subroutine get_key_value_datetime_fortran(table, key, val, default, stat, origin)
    !> Instance of the TOML table
    class(toml_table), intent(inout) :: table
    !> Key in this TOML table
    type(toml_key), intent(in) :: key
    !> Datetime value
    type(datetime), intent(out) :: val
    !> Default datetime value
    type(datetime), intent(in), optional :: default
    !> Status of operation
    integer, intent(out), optional :: stat
    !> Origin in the data structure
    integer, intent(out), optional :: origin

    call get_value(table, key%key, val, default, stat, origin)
  end subroutine get_key_value_datetime_fortran

  !> Retrieve TOML value as datetime
  subroutine get_elem_value_datetime_fortran(array, pos, val, stat, origin)
    !> Instance of the TOML array
    class(toml_array), intent(inout) :: array
    !> Position in the array
    integer, intent(in) :: pos
    !> Integer value
    type(datetime), intent(out) :: val
    !> Status of operation
    integer, intent(out), optional :: stat
    !> Origin in the data structure
    integer, intent(out), optional :: origin

    type(toml_keyval), pointer :: ptr

    call get_value(array, pos, ptr, stat, origin)

    if (associated(ptr)) then
      call get_value(ptr, val, stat, origin)
    else
      if (present(stat)) stat = toml_stat%fatal
    end if
  end subroutine get_elem_value_datetime_fortran

  !> Convert to a TOML representation of a datetime object
  elemental subroutine convert_to_datetime(lhs, rhs)
    !> TOML representation of datetime
    type(toml_datetime), intent(out) :: lhs
    !> Datetime-fortran representation of datetime
    type(datetime), intent(in) :: rhs

    lhs%date%year = rhs%getYear()
    lhs%date%month = rhs%getMonth()
    lhs%date%day = rhs%getDay()
    lhs%time%hour = rhs%getHour()
    lhs%time%minute = rhs%getMinute()
    lhs%time%second = rhs%getSecond()
    lhs%time%msec = rhs%getMillisecond() * 1000
  end subroutine convert_to_datetime

  !> Convert from a TOML representation of a datetime object
  elemental subroutine convert_from_datetime(lhs, rhs)
    !> Datetime-fortran representation of datetime
    type(datetime), intent(out) :: lhs
    !> TOML representation of datetime
    type(toml_datetime), intent(in) :: rhs

    integer, allocatable :: year, month, day, hour, minute, second, millisecond

    if (rhs%date%year > -1) year = rhs%date%year
    if (rhs%date%month > -1) month = rhs%date%month
    if (rhs%date%day > -1) day = rhs%date%day
    if (rhs%time%hour > -1) hour = rhs%time%hour
    if (rhs%time%minute > -1) minute = rhs%time%minute
    if (rhs%time%second > -1) second = rhs%time%second
    if (rhs%time%msec > -1) millisecond = rhs%time%msec / 1000

    lhs = datetime(year=year, month=month, day=day, hour=hour, minute=minute, &
      & second=second, millisecond=millisecond)
  end subroutine convert_from_datetime

end module demo_compat
