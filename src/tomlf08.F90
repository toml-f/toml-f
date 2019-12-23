module tomlf08
   use tomlf08_constants
   use tomlf08_type
   use tomlf08_utils
   use tomlf08_ser
   use tomlf08_de
   implicit none

contains

subroutine tomlf08_version(vstr)
   character(len=:), allocatable, intent(out) :: vstr
   vstr = "0.1"
end subroutine tomlf08_version

end module tomlf08
