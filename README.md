# TOML parser for Fortran projects

[![License](https://img.shields.io/badge/license-MIT%7CApache%202.0-blue)](LICENSE-Apache)
[![Release](https://img.shields.io/github/v/release/toml-f/toml-f)](https://github.com/toml-f/toml-f/releases/latest)
[![Build](https://github.com/toml-f/toml-f/workflows/CI/badge.svg)](https://github.com/toml-f/toml-f/actions)
[![docs](https://github.com/toml-f/toml-f/workflows/docs/badge.svg)](https://toml-f.github.io/toml-f)
[![Documentation Status](https://readthedocs.org/projects/toml-f/badge/?version=latest)](https://toml-f.readthedocs.io)
[![codecov](https://codecov.io/gh/toml-f/toml-f/branch/master/graph/badge.svg)](https://codecov.io/gh/toml-f/toml-f)

A TOML parser implementation for data serialization and deserialization in Fortran.

* the [TOML standard](https://toml.io)
* currently supported [TOML v1.0.0 specification](https://toml.io/en/v1.0.0)

<div align="center">
<img src="./assets/toml-f.png" alt="TOML-Fortran" width="220">
</div>


## Installation

To build this project from the source code in this repository you need to have

- a Fortran compiler supporting Fortran 2008 (GCC 5 or newer or Intel Fortran)
- One of the supported build systems

  - [meson](https://mesonbuild.com) version 0.53 or newer
  - [CMake](https://cmake.org/) version 3.9 or newer and
  - [Fortran package manager (fpm)](https://github.com/fortran-lang/fpm) version 0.2.0 or newer


### Meson

To build this project with meson a build-system backend is required, *i.e.* [ninja](https://ninja-build.org) version 1.7 or newer.
Setup a build with

```
meson setup _build
```

You can select the Fortran compiler by the `FC` environment variable.
To compile the project run

```
meson compile -C _build
```

To use `toml-f` in your project, have a look at the [example integration with meson](https://github.com/toml-f/tf-meson-example).

We employ a [validator suite](https://github.com/BurntSushi/toml-test) to test the standard compliance of this implementation.
To use this testing a `go` installation is required.
The installation of the validator suite will be handled by meson automatically without installing into the users `go` workspace.
Run the tests with

```
meson test -C _build --print-errorlogs
```

To run the full decoder test add the benchmark argument.
This test will currently fail, due to the implementation not yet supporting Unicode escape sequences.

```
meson test -C _build --benchmark --print-errorlogs
```

The binary used for transcribing the TOML documents to the testing format is `_build/test/toml2json` and can be used to check on per test basis.


### CMake

While meson is the preferred way to build this project it also offers CMake support.
Configure the CMake build with

```
cmake -S. -B_build -GNinja
```

Similar to meson the compiler can be selected with the `FC` environment variable.
You can build the project using

```
cmake --build _build
```

To include `toml-f` in your CMake project, check the [example integration with CMake](https://github.com/toml-f/tf-cmake-example).
The validation suite is currently not supported as unit test for CMake builds and requires a manual setup instead using the `toml2json` binary.


### Fortran package manager

The Fortran package manager [`fpm`](https://github.com/fortran-lang/fpm) supports the addition of `toml-f` as a dependency. In the `fpm.toml` file:

```toml
[dependencies]
toml-f.git = "https://github.com/toml-f/toml-f"
```

Then build and test normally.

```
fpm build
fpm test
```

A more detailed example is described in [example 1](test/example-1).


## Documentation

To build the documentation with `ford` run

```
ford -o ./docs docs.md
```


## Usage

To make use this library use the `tomlf` module in your projects.
You can access the individual modules but those are not considered part of the public API and might change between versions.

An example program to load and dump a TOML file would look like this:

```fortran
use tomlf
implicit none
character(len=1), parameter :: nl = new_line('a')
type(toml_table), allocatable :: table
character(kind=tfc, len=:), allocatable :: input_string
type(toml_serializer) :: ser

input_string = &
   & '# This is a TOML document.' // nl // &
   & 'title = "TOML Example"' // nl // &
   & '[owner]' // nl // &
   & 'name = "Tom Preston-Werner"' // nl // &
   & 'dob = 1979-05-27T07:32:00-08:00 # First class dates' // nl // &
   & '[database]' // nl // &
   & 'server = "192.168.1.1"' // nl // &
   & 'ports = [ 8001, 8001, 8002 ]' // nl // &
   & 'connection_max = 5000' // nl // &
   & 'enabled = true' // nl // &
   & '[servers]' // nl // &
   & '  # Indentation (tabs and/or spaces) is allowed but not required' // nl // &
   & '  [servers.alpha]' // nl // &
   & '  ip = "10.0.0.1"' // nl // &
   & '  dc = "eqdc10"' // nl // &
   & '  [servers.beta]' // nl // &
   & '  ip = "10.0.0.2"' // nl // &
   & '  dc = "eqdc10"' // nl // &
   & '[clients]' // nl // &
   & 'data = [ ["gamma", "delta"], [1, 2] ]' // nl // &
   & '# Line breaks are OK when inside arrays' // nl // &
   & 'hosts = [' // nl // &
   & '  "alpha",' // nl // &
   & '  "omega"' // nl // &
   & ']'

call toml_parse(table, input_string)
if (allocated(table)) then
   call table%accept(ser)
   call table%destroy  ! not necessary
end if
end
```

Here the TOML document is provided as string, notice that you have to add a newline character by using the intrinsic function `new_line("a")` to get the lines correctly.

Alternatively, a file can be loaded from any connected, formatted unit using the same overloaded function.
For the standard input the intrinsic `input_unit` should be passed.
If the TOML file is successfully parsed the table will be allocated and can be written to the standard output by passing the `toml_serializer` as visitor to the table.

Additionally, detailed examples are provided as well:

- simple reader for a configuration file using `fpm`, [see example 1](test/example-1)


## Contributing

See the [contributing guidelines](CONTRIBUTING.md) on how to get involved in TOML-Fortran.


## License

TOML-Fortran is free software: you can redistribute it and/or modify it under the terms of the [Apache License, Version 2.0](LICENSE-Apache) or [MIT license](LICENSE-MIT) at your opinion.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an _as is_ basis, without warranties or conditions of any kind, either express or implied. See the License for the specific language governing permissions and limitations under the License.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in TOML-Fortran by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
