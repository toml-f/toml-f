program array
  use tomlf, only : toml_table, toml_array, get_value, &
                  & toml_parse, toml_error, len

  implicit none
  integer, dimension(:), allocatable  :: arr_data
  integer                             :: data_len, io, i
  type(toml_table), allocatable       :: table
  type(toml_array), pointer           :: arr
  type(toml_error), allocatable       :: parse_error

  open(newunit=io, file="array.toml")
  call toml_parse(table, io, parse_error)
  close(unit=io)
  if (allocated(parse_error)) then
    print*, "Error parsing table:"//parse_error%message
  end if

  ! Get the value of "data" by parsing the array and then looping over its
  ! elements
  call get_value(table, "data", arr)
  if (associated(arr)) then
    data_len = len(arr)
    allocate(arr_data(data_len))
    do i = 1,data_len
      call get_value(arr, i, arr_data(i))
    end do
  end if

  print*, "data = ", arr_data

end program
