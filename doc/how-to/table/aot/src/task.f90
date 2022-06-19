!> Example for a workflow pipeline to construct a molecular dynamics simulation.
module demo_task
  use tomlf, only : toml_table, toml_array, get_value, len
  implicit none
  private

  public :: read_tasks, task_config, driver_config, lbfgs_config, velocity_verlet_config

  !> Abstract base class for all simulation driver configurations
  type, abstract :: driver_config
  end type driver_config

  !> Configuration for the LBFGS geometry optimization driver
  type, extends(driver_config) :: lbfgs_config
    !> Tolerance for considering optimization to be converged
    real :: tolerance
  end type lbfgs_config

  !> Configuration for the Velocity-Verlet molecular dynamics driver
  type, extends(driver_config) :: velocity_verlet_config
    !> Time step for the propagation in fs
    real :: time_step
    !> Temperature in K
    real :: temperature
    !> Number of steps to take in the propagation
    integer :: max_steps
  end type velocity_verlet_config

  !> Configuration of a single simulation task
  type :: task_config
    !> Label to identify the task
    character(len=:), allocatable :: label
    !> Driver configuration
    class(driver_config), allocatable :: config
  end type task_config

contains

  !> Read task configurations from document root
  subroutine read_tasks(table, task)
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Configurations for simulation tasks
    type(task_config), allocatable, intent(out) :: task(:)

    integer :: itask
    type(toml_array), pointer :: array
    type(toml_table), pointer :: child

    call get_value(table, "tasks", array)
    allocate(task(len(array)))

    do itask = 1, size(task)
      call get_value(array, itask, child)
      call read_task(child, task(itask))
    end do
  end subroutine read_tasks

  !> Read a single task configuration
  subroutine read_task(table, task)
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Configuration for simulation task
    type(task_config), intent(out) :: task

    character(len=:), allocatable :: driver
    type(toml_table), pointer :: child

    call get_value(table, "driver", driver)
    if (.not.allocated(driver)) then
      error stop "Driver keyword not present in task configuration"
    end if
    call get_value(table, "name", task%label, driver)

    select case(driver)
    case default
      error stop "Unknown driver type in task configuration"
    case("lbfgs")
      block
        type(lbfgs_config), allocatable :: tmp
        allocate(tmp)
        call get_value(table, "config", child)
        call read_lbfgs(child, tmp)
        call move_alloc(tmp, task%config)
      end block
    case("velocity-verlet")
      block
        type(velocity_verlet_config), allocatable :: tmp
        allocate(tmp)
        call get_value(table, "config", child)
        call read_velocity_verlet(child, tmp)
        call move_alloc(tmp, task%config)
      end block
    end select
  end subroutine read_task

  !> Read configuration for LBFGS geometry optimization
  subroutine read_lbfgs(table, config)
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Configuration for LBFGS optimizer
    type(lbfgs_config), intent(out) :: config

    call get_value(table, "tolerance", config%tolerance, 1.0e-6)
  end subroutine read_lbfgs

  !> Read configuration for Velocity-Verlet molecular dynamics
  subroutine read_velocity_verlet(table, config)
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Configuration for Velocity-Verlet propagator
    type(velocity_verlet_config), intent(out) :: config

    call get_value(table, "time-step", config%time_step, 0.5)
    call get_value(table, "temperature", config%temperature, 298.15)
    call get_value(table, "max-steps", config%max_steps, 100)
  end subroutine read_velocity_verlet

end module demo_task
