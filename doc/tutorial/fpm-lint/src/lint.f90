!> Linter for package manifests used with the Fortran package manager
module fpm_lint
  use fpm_lint_config, only : lint_config, load_lint_config
  use fpm_lint_logger, only : lint_logger, new_logger
  use fpm_lint_utils, only : resize, get_argument
  use tomlf, only : toml_table, toml_context, toml_terminal, toml_error, toml_level, &
    & toml_key, get_value
  use tomlf_de_token, only : token_kind, stringify
  implicit none
  private

  public :: lint_config, load_lint_config
  public :: lint_logger, new_logger
  public :: lint_data
  public :: lint_keys
  public :: get_argument

contains

  !> Entry point for linting the data structure representing the package manifest
  subroutine lint_data(logger, config, table, context, terminal)
    !> Instance of the logger
    type(lint_logger), intent(inout) :: logger
    !> Configuration for the linter
    type(lint_config), intent(in) :: config
    !> TOML data structure
    type(toml_table), intent(inout) :: table
    !> Context describing the data structure
    type(toml_context), intent(in) :: context
    !> Terminal for output
    type(toml_terminal), intent(in) :: terminal

    if (config%package_name) then
      check_package_name: block
        character(:), allocatable :: package_name
        integer :: origin

        call get_value(table, "name", package_name, origin=origin)

        if (.not.allocated(package_name)) then
          call logger%add_message(context%report( &
            "Package name entry is required in the top-level", &
            origin, &
            level=toml_level%info, color=terminal))
          exit check_package_name
        end if

        if (context%token(origin)%kind /= token_kind%string) then
          call logger%add_message(context%report( &
            "Package name should not be a "//stringify(context%token(origin)), &
            origin, &
            "prefer string value (double quotes)", &
            level=toml_level%info, color=terminal))
        end if

        if (verify(package_name, "abcdefghijklmnopqrstuvwxyz0123456789-") > 0) then
          call logger%add_message(context%report( &
            "Package name should be lowercase with dashes", &
            origin, &
            level=toml_level%info, color=terminal))
        end if
      end block check_package_name
    end if

  end subroutine lint_data

  !> Entry point for linting the keys in the TOML document
  subroutine lint_keys(logger, config, context, terminal)
    !> Instance of the logger
    type(lint_logger), intent(inout) :: logger
    !> Configuration for the linter
    type(lint_config), intent(in) :: config
    !> Context describing the data structure
    type(toml_context), intent(in) :: context
    !> Terminal for output
    type(toml_terminal), intent(in) :: terminal

    integer :: it
    type(toml_key), allocatable :: list(:)

    call identify_keys(list, context)

    if (config%bare_keys) then
      do it = 1, size(list)
        associate(token => context%token(list(it)%origin))
          if (token%kind /= token_kind%keypath) then
            call logger%add_message(context%report( &
              "String used in key path", &
              list(it)%origin, &
              "use bare key instead", &
              level=toml_level%info, color=terminal))
          end if
        end associate
      end do
    end if
  end subroutine lint_keys

  !> Collect all key paths used in TOML document
  subroutine identify_keys(list, context)
    !> List of all keypaths in the TOML document
    type(toml_key), allocatable, intent(out) :: list(:)
    !> Context describing the data structure
    type(toml_context), intent(in) :: context

    integer, parameter :: table_scope = 1, array_scope = 2, value_scope = 3
    integer :: it, top
    integer, allocatable :: scopes(:)

    allocate(list(0))

    top = 0
    call resize(scopes)

    ! Documents always start with a table scope
    call push_back(scopes, top, table_scope)
    do it = 1, context%top
      select case(context%token(it)%kind)
      case(token_kind%keypath)
        ! Record all key path
        associate(token => context%token(it))
          list = [list, toml_key(context%source(token%first:token%last), it)]
        end associate

      case(token_kind%string, token_kind%literal)
        ! Record all strings used in key paths
        if (scopes(top) == table_scope) then
          associate(token => context%token(it))
            list = [list, toml_key(context%source(token%first+1:token%last-1), it)]
          end associate
        end if

      case(token_kind%equal)  ! Open value scope
        call push_back(scopes, top, value_scope)

      case(token_kind%lbrace)  ! Open inline table scope
        call push_back(scopes, top, table_scope)

      case(token_kind%lbracket)  ! Open array scope
        if (scopes(top) /= table_scope) then
          call push_back(scopes, top, array_scope)
        end if

      case(token_kind%newline)  ! Close value scope in key-value pair 
        call pop(scopes, top, value_scope)

      case(token_kind%rbrace)  ! Close value and table scope in inline table
        call pop(scopes, top, value_scope)
        call pop(scopes, top, table_scope)

      case(token_kind%comma)  ! Close value scope in inline table
        call pop(scopes, top, value_scope)

      case(token_kind%rbracket)  ! Close array scope
        call pop(scopes, top, array_scope)

      end select
    end do

  contains

    !> Push a new scope onto the stack
    pure subroutine push_back(scopes, top, this_scope)
      !> Stack top
      integer, intent(inout) :: top
      !> Current stack of scopes
      integer, allocatable, intent(inout) :: scopes(:)
      !> Scope to push onto the stack
      integer, intent(in) :: this_scope

      top = top + 1
      if (top > size(scopes)) call resize(scopes)
      scopes(top) = this_scope
    end subroutine push_back

    !> Remove a matching scope from the stack
    subroutine pop(scopes, top, this_scope)
      !> Stack top
      integer, intent(inout) :: top
      !> Current stack of scopes
      integer, allocatable, intent(inout) :: scopes(:)
      !> Scope to remove from the stack
      integer, intent(in) :: this_scope

      if (top > 0) then
        if (scopes(top) == this_scope) top = top - 1
      end if
    end subroutine pop

  end subroutine identify_keys

end module fpm_lint
