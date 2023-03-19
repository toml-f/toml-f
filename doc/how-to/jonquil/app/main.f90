program demo
  use jonquil, only : json_loads, json_value, json_object, json_error, cast_to_object, &
    & get_value
  implicit none

  class(json_value), allocatable :: val
  type(json_object), pointer :: object
  type(json_error), allocatable :: error
  integer :: ival

  call json_loads(val, '{"a":1,"b":2}', error=error)
  if (allocated(error)) then
    print '(a)', error%message
    stop
  end if

  object => cast_to_object(val)
  if (associated(object)) then
    call get_value(object, "a", ival)
    print '(a,1x,i0)', "a is", ival

    call get_value(object, "b", ival)
    print '(a,1x,i0)', "b is", ival
  end if

  block
    use tomlf, only : toml_table, toml_array, add_array, set_value, toml_serialize

    integer :: it
    type(toml_table), pointer :: table
    type(toml_array), pointer :: array

    ! Add an array to the object
    call add_array(object, "c", array)
    call set_value(array, [-1, 0, 1])

    ! Create a new subobject / subtable
    call get_value(object, "d", table, requested=.true.)
    call set_value(table, "sub", "table")

    print '(a)', "# representation in TOML land"
    print '(a)', toml_serialize(object)
  end block

end program demo
