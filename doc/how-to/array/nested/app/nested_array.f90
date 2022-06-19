program array
  use tomlf, only : toml_table, toml_array, get_value, &
        & toml_parse, toml_error, len

  implicit none
  integer                             :: top_len, nested_len, io, i, j, elem
  type(toml_table), allocatable       :: table
  type(toml_array), pointer           :: top_array, nested_array
  type(toml_error), allocatable       :: parse_error
  
  open(newunit=io, file="nested_array.toml")
  call toml_parse(table, io, parse_error)
  close(unit=io)
  if (allocated(parse_error)) then
    print*, "Error parsing table:"//parse_error%message
  end if

  ! Get the top-level array and loop over its elements. Each element is an
  ! array of variable size
  call get_value(table, "data", top_array)
  if (associated(top_array)) then
    top_len = len(top_array)
    do i = 1,top_len
      ! Now get a pointer to the i'th nested array
      call get_value(top_array, i, nested_array)
      if (associated(nested_array)) then
        nested_len = len(nested_array)
        ! And get the elements of the nested array
        do j = 1,nested_len
          call get_value(nested_array, j, elem)
          ! Do something with "elem" here
          write(*,'(A5,i1,A1,i1,A3,i2)') "data(",i,",",j,") = ",elem
        end do
      end if
    end do
  end if

end program
