program tester
  use tomlf_version, only : get_tomlf_version
  implicit none
  character(len=:), allocatable :: version
  call get_tomlf_version(string=version)
  print *, version
end program tester
