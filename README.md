# TOML parser for Fortran projects

[![Build Status](https://travis-ci.com/awvwgk/toml-f.svg?branch=master)](https://travis-ci.com/awvwgk/toml-f)
[![Build Status](https://github.com/awvwgk/toml-f/workflows/CI/badge.svg)](https://github.com/awvwgk/toml-f/actions)

A TOML parser implementation for data serialization and deserialization in Fortran.

- the [TOML standard](https://github.com/toml-lang/toml)
- currently supported [TOML v0.5 specification](https://github.com/toml-lang/toml/blob/v0.5.0/README.md)

<img src="./assets/toml-f.png" alt="TOML-Fortran" width="240">

## Installation

To build this project from the source code in this repository you need to have
- a Fortran compiler supporting Fortran 2008
- `meson` version 0.49 or newer, see https://mesonbuild.com/
- `ninja` version 1.5 or newer, see https://ninja-build.org/
- `fypp` version 2 or newer, see https://github.com/aradi/fypp/

Setup a build with

```bash
meson setup build_gcc
ninja -C build_gcc
```

### Testing

To start the testsuite (requires a `go` installation) run

```bash
ninja -C build_gcc test
```

which will currently *fail*.

### Documentation

To build the documentation with `ford` run

```bash
ninja -C build_gcc docs
```

The resulting documentation can be found at `./build_gcc/docs`.
Alternatively it can be build directly with `ford` by running

```bash
ford -o ./docs docs.md
```

## Usage

To make use this library use the `tomlf08` module in your projects.
An example program to load and dump a TOML file would look like this:

```fortran
use tomlf08
implicit none
type(toml_table), allocatable :: table
character(len=:), allocatable :: input_string
type(toml_serializer) :: ser

input_string = &
   & '# This is a TOML document.' // TOML_NEWLINE // &
   & 'title = "TOML Example"' // TOML_NEWLINE // &
   & '[owner]' // TOML_NEWLINE // &
   & 'name = "Tom Preston-Werner"' // TOML_NEWLINE // &
   & 'dob = 1979-05-27T07:32:00-08:00 # First class dates' // TOML_NEWLINE // &
   & '[database]' // TOML_NEWLINE // &
   & 'server = "192.168.1.1"' // TOML_NEWLINE // &
   & 'ports = [ 8001, 8001, 8002 ]' // TOML_NEWLINE // &
   & 'connection_max = 5000' // TOML_NEWLINE // &
   & 'enabled = true' // TOML_NEWLINE // &
   & '[servers]' // TOML_NEWLINE // &
   & '  # Indentation (tabs and/or spaces) is allowed but not required' // TOML_NEWLINE // &
   & '  [servers.alpha]' // TOML_NEWLINE // &
   & '  ip = "10.0.0.1"' // TOML_NEWLINE // &
   & '  dc = "eqdc10"' // TOML_NEWLINE // &
   & '  [servers.beta]' // TOML_NEWLINE // &
   & '  ip = "10.0.0.2"' // TOML_NEWLINE // &
   & '  dc = "eqdc10"' // TOML_NEWLINE // &
   & '[clients]' // TOML_NEWLINE // &
   & 'data = [ ["gamma", "delta"], [1, 2] ]' // TOML_NEWLINE // &
   & '# Line breaks are OK when inside arrays' // TOML_NEWLINE // &
   & 'hosts = [' // TOML_NEWLINE // &
   & '  "alpha",' // TOML_NEWLINE // &
   & '  "omega"' // TOML_NEWLINE // &
   & ']'

call toml_parse(table, input_string)
if (allocated(table)) then
   call table%accept(ser)
   call table%destroy  ! not necessary
end if
end
```

Here the TOML file is provided as string, notice that you have to add a
newline character either by the parameter `TOML_NEWLINE` or by using the
intrinsic function `new_line('a')` to get the correct newline characters.

Alternatively a file can be loaded from any connected, formatted unit using
the same overloaded function. For the standard input the intrinsic `input_unit`
should be passed. If the TOML file is successfully parsed the table will
be allocated and can be written to the standard output by passing the
`toml_serializer` as visitor to the table.

### as subproject

`meson` based projects can use `toml-f` as subproject with this wrap-file:

```
[wrap-git]
directory = toml-f
url = https://github.com/awvwgk/toml-f.git
revision = head
```

Despite looking similar to a TOML file the wrap-file is not valid TOML.

Place this file in your `subprojects` directory and add

```meson
tomlf_dep = dependency('toml-f', ['toml-f', 'tomlf_dep'])
```

to your `meson.build` file.

## License

TOML-Fortran is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TOML-Fortran is distributed in the hope that it will be useful,
but without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  See the
GNU General Public License for more details.
