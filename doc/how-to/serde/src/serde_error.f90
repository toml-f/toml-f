!> Central registry for error codes
module serde_error
  implicit none
  private

  public :: error_type, fatal_error

  !> Possible error codes
  type :: enum_stat
    !> Successful run
    integer :: success = 0
    !> Internal error:
    integer :: fatal = 1
  end type enum_stat

  !> Actual enumerator for return states
  type(enum_stat), parameter :: serde_stat = enum_stat()

  !> Error message
  type :: error_type
    !> Error code
    integer :: stat
    !> Payload of the error
    character(len=:), allocatable :: message
  end type error_type

contains

  !> A fatal error is encountered
  subroutine fatal_error(error, message, stat)
    !> Instance of the error
    type(error_type), allocatable, intent(out) :: error
    !> A detailed message describing the error and (optionally) offering advice
    character(len=*), intent(in), optional :: message
    !> Overwrite of the error code
    integer, intent(in), optional :: stat

    allocate(error)

    if (present(stat)) then
      error%stat = stat
    else
      error%stat = serde_stat%fatal
    end if

    if (present(message)) then
      error%message = message
    else
      error%message = "Fatal error"
    end if
  end subroutine fatal_error

end module serde_error
