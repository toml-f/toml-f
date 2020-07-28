program tomlf_example
   use iso_fortran_env
   use tomlf_de, only : toml_parse
   use tomlf_ser, only : toml_serializer
   use tomlf_type, only : toml_table
   implicit none
   integer :: iarg, length
   character(len=:), allocatable :: argument
   type(toml_table), allocatable :: table
   type(toml_serializer) :: ser
   integer :: unit
   logical :: exist

   if (command_argument_count() > 0) then
      do iarg = 1, command_argument_count()
         if (allocated(argument)) deallocate(argument)
         call get_command_argument(iarg, length=length)
         allocate(character(len=length) :: argument)
         call get_command_argument(iarg, argument)
         inquire(file=argument, exist=exist)
         if (exist) then
            open(newunit=unit, file=argument)
            print'("#",1x,a)', argument
            if (allocated(table)) deallocate(table)
            call toml_parse(table, unit)
            close(unit)
            if (allocated(table)) then
               call table%accept(ser)
               call table%destroy
            end if
         end if
      end do
   end if
   if (allocated(argument)) deallocate(argument)

end program tomlf_example
