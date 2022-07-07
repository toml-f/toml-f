module fpm_lint_logger
  implicit none
  private

  public :: lint_logger, new_logger


  type :: log_message
    character(:), allocatable :: output
  end type log_message

  type :: lint_logger
    type(log_message), allocatable :: message(:)
  contains
    procedure :: add_message
    procedure :: show_log
  end type lint_logger

contains

  subroutine new_logger(logger)
    type(lint_logger), intent(out) :: logger

    allocate(logger%message(0))
  end subroutine new_logger

  subroutine add_message(logger, message)
    class(lint_logger), intent(inout) :: logger
    character(*), intent(in) :: message

    logger%message = [logger%message, log_message(message)]
  end subroutine add_message

  subroutine show_log(logger, io)
    class(lint_logger), intent(in) :: logger
    integer, intent(in) :: io

    integer :: it

    do it = 1, size(logger%message)
      write(io, '(a)') logger%message(it)%output
    end do
  end subroutine show_log

end module fpm_lint_logger
