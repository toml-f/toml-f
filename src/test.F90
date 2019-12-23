program tester
   use iso_fortran_env
   !use tomlc99
   use tomlf08
   implicit none
   integer :: iarg, length
   character(len=:), allocatable :: argument
   integer :: unit
   type(toml_table_t), allocatable :: table
   type(toml_serializer_t) :: ser
   logical :: stat, exist
   real(TOML_FLOAT_KIND) :: dum

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

end program
