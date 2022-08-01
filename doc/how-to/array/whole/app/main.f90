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

  call get_value(table, "data", arr)
  if (associated(arr)) then
    call get_value(arr, arr_data)
  end if

  print*, "data = ", arr_data

end program

