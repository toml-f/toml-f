program readin
  use reader, only : read_data
  use tomlf, only : toml_table, toml_parse, toml_error
  implicit none

  type(toml_table), allocatable :: table
  character(len=:), allocatable :: title
  real, allocatable :: spectrum(:)


  block
    integer :: io
    type(toml_error), allocatable :: error

    open(file="input.toml", newunit=io, status="old")
    call toml_parse(table, io, error)
    close(io)
    if (allocated(error)) then
      print '(a)', "Error: "//error%message
      stop 1
    end if
  end block

  call read_data(table, title, spectrum)

  if (allocated(title)) then
    print '(a)', "Title: '"//title//"'"
  end if

  print '(*(g0, 1x))', "Entries:", size(spectrum)
  if (size(spectrum) > 0) then
    print '(*(g0, 1x))', "Spectrum:", spectrum
  end if
end program readin
