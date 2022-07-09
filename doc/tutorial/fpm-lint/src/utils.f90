!> Misc utilities for the fpm-lint implementation
module fpm_lint_utils
  implicit none
  private

  public :: resize
  public :: get_argument

  !> Resize a 1D array to a new size
  interface resize
    module procedure :: resize_ints
  end interface resize

contains

  !> Reallocate list of integer
  pure subroutine resize_ints(var, n)
    !> Instance of the array to be resized
    integer, allocatable, intent(inout) :: var(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    integer, allocatable :: tmp(:)
    integer :: this_size, new_size
    integer, parameter :: initial_size = 8

    if (allocated(var)) then
      this_size = size(var, 1)
      call move_alloc(var, tmp)
    else
      this_size = initial_size
    end if

    if (present(n)) then
      new_size = n
    else
      new_size = this_size + this_size/2 + 1
    end if

    allocate(var(new_size))

    if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(var, 1))
      var(:this_size) = tmp(:this_size)
      deallocate(tmp)
    end if
  end subroutine resize_ints

  !> Obtain the command line argument at a given index
  subroutine get_argument(idx, arg)
    !> Index of command line argument, range [0:command_argument_count()]
    integer, intent(in) :: idx
    !> Command line argument
    character(len=:), allocatable, intent(out) :: arg

    integer :: length, stat

    call get_command_argument(idx, length=length, status=stat)
    if (stat == 0) then
      allocate(character(len=length) :: arg, stat=stat)
    end if

    if (stat == 0 .and. length > 0) then
      call get_command_argument(idx, arg, status=stat)
      if (stat /= 0) deallocate(arg)
    end if
  end subroutine get_argument

end module fpm_lint_utils
