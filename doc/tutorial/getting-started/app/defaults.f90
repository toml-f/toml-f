program defaults
  use reader, only : read_data
  use tomlf, only : toml_table
  implicit none

  type(toml_table), allocatable :: table
  character(len=:), allocatable :: title
  real, allocatable :: spectrum(:)

  table = toml_table()

  call read_data(table, title, spectrum)

  if (allocated(title)) then
    print '(a)', "Title: '"//title//"'"
  end if

  print '(*(g0, 1x))', "Entries:", size(spectrum)
  if (size(spectrum) > 0) then
    print '(*(g0, 1x))', "Spectrum:", spectrum
  end if
end program defaults
