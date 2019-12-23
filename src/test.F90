program tester
   use iso_fortran_env
   !use tomlc99
   use tomlf08
   implicit none
   integer :: iarg, length
   character(len=:), allocatable :: argument
   integer :: unit
   type(toml_table_t), allocatable :: table
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
            print'(a)', argument
            if (allocated(table)) deallocate(table)
            call toml_parse(table, unit)
            close(unit)
            if (allocated(table)) then
               call print_table(output_unit, table)
               call table%destroy
            end if
         end if
      end do
   end if
   if (allocated(argument)) deallocate(argument)

end program
