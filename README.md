# TOML parser for Fortran projects

[![License](https://img.shields.io/badge/license-MIT%7CApache%202.0-blue)](LICENSE-Apache)
[![Build](https://travis-ci.com/awvwgk/toml-f.svg?branch=master)](https://travis-ci.com/awvwgk/toml-f)
[![Build](https://github.com/awvwgk/toml-f/workflows/CI/badge.svg)](https://github.com/awvwgk/toml-f/actions)

A TOML parser implementation for data serialization and deserialization in Fortran.

- the [TOML standard](https://toml.io)
- currently supported [TOML v1.0.0-rc1 specification](https://toml.io/en/v1.0.0-rc.1)

<div align="center">
<img src="./assets/toml-f.png" alt="TOML-Fortran" width="220">
</div>


## Installation

To build this project from the source code in this repository you need to have
- a Fortran compiler supporting Fortran 2008
- [`meson`](https://mesonbuild.com) version 0.49 or newer
- a build-system backend, *i.e.* [`ninja`](https://ninja-build.org) version 1.7 or newer

Setup a build with

```
FC=gfortran meson setup build_gcc
meson compile -C build
```


### Testing

_Work in progress_


### Documentation

To build the documentation with `ford` run

```
ford -o ./docs docs.md
```


## Usage

To make use this library use the `tomlf` module in your projects,
for the complete public API use `tomlf_all`, you can access the indiviual modules
but those are not considered part of the public API and might change between
versions.

An example program to load and dump a TOML file would look like this:

```fortran
use tomlf
implicit none
type(toml_table), allocatable :: table
character(kind=tfc, len=:), allocatable :: input_string
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
intrinsic function `new_line("a")` to get the correct newline characters.

Alternatively, a file can be loaded from any connected, formatted unit using
the same overloaded function. For the standard input the intrinsic `input_unit`
should be passed. If the TOML file is successfully parsed the table will
be allocated and can be written to the standard output by passing the
`toml_serializer` as visitor to the table.


## Contributing

See the [contributing guidelines](CONTRIBUTING.md) on how to get involved
in TOML-Fortran.


## License

TOML-Fortran is free software: you can redistribute it and/or modify it under
the terms of the [Apache License, Version 2.0](LICENSE-Apache) or
[MIT license](LICENSE-MIT) at your option.

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an _as is_ basis, without warranties or
conditions of any kind, either express or implied. See the License for the
specific language governing permissions and limitations under the License.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in TOML-Fortran by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions. 
