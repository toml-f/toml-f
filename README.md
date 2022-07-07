# TOML parser for Fortran projects

[![License](https://img.shields.io/badge/license-MIT%7CApache%202.0-blue)](LICENSE-Apache)
[![Release](https://img.shields.io/github/v/release/toml-f/toml-f)](https://github.com/toml-f/toml-f/releases/latest)
[![Build](https://github.com/toml-f/toml-f/workflows/CI/badge.svg)](https://github.com/toml-f/toml-f/actions)
[![docs](https://github.com/toml-f/toml-f/workflows/docs/badge.svg)](https://toml-f.github.io/toml-f)
[![Documentation Status](https://readthedocs.org/projects/toml-f/badge/?version=latest)](https://toml-f.readthedocs.io)
[![codecov](https://codecov.io/gh/toml-f/toml-f/branch/main/graph/badge.svg)](https://codecov.io/gh/toml-f/toml-f)

A TOML parser implementation for data serialization and deserialization in Fortran.

* the [TOML standard](https://toml.io)
* currently supported [TOML v1.0.0 specification](https://toml.io/en/v1.0.0)
* [TOML Fortran documentation](https://toml-f.readthedocs.io)

<div align="center">
<img src="./assets/toml-f.png" alt="TOML-Fortran" width="220">
</div>


## Installation

The TOML Fortran library is available via various distribution channels.
If your channel is not found here, please checkout the instructions for [building from source](#building-from-source).
You can also find these instructions in the user documentation at [Installing TOML Fortran](https://toml-f.readthedocs.io/en/latest/how-to/installation.html).


### Conda package

[![Conda](https://img.shields.io/conda/vn/conda-forge/toml-f)](https://github.com/conda-forge/toml-f-feedstock)
[![Conda](https://img.shields.io/conda/pn/conda-forge/toml-f)](https://github.com/conda-forge/toml-f-feedstock)

This project is packaged for the *mamba* package manager and available on the *conda-forge* channel.
To install the *mamba* package manager we recommend the [mambaforge](https://github.com/conda-forge/miniforge/releases) installer.
If the *conda-forge* channel is not yet enabled, add it to your channels with

```
mamba config --add channels conda-forge
mamba config --set channel_priority strict
```

Once the *conda-forge* channel has been enabled, TOML Fortran can be installed with *mamba*:

```
mamba install toml-f
```

It is possible to list all of the versions of TOML Fortran available on your platform with *mamba*:

```
mamba repoquery search toml-f --channel conda-forge
```


### FreeBSD port

[![FreeBSD port](https://repology.org/badge/version-for-repo/freebsd/toml-f.svg)](https://www.freshports.org/textproc/toml-f/)

A port for FreeBSD is available and can be installed using

```
pkg install textproc/toml-f
```

In case no package is available build the port using

```
cd /usr/ports/textproc/toml-f
make install clean
```

For more information see the [toml-f port details](https://www.freshports.org/textproc/toml-f/).


### Alternative distributions

Please let us know if you are packaging TOML Fortran.
Other available distributions of TOML Fortran currently include

- Homebrew tap at [grimme-lab/homebrew-qc](https://github.com/grimme-lab/homebrew-qc)
- Easy-build config at [easybuilders/easybuild-easyconfigs](https://github.com/easybuilders/easybuild-easyconfigs/tree/develop/easybuild/easyconfigs/t/TOML-Fortran)
- spack package in [develop](https://github.com/spack/spack/blob/develop/var/spack/repos/builtin/packages/toml-f/package.py)

An overview of the availability of TOML Fortran in distributions tracked by [Repology](https://repology.org) is provided here:

[![Packaging status](https://repology.org/badge/vertical-allrepos/toml-f.svg)](https://repology.org/project/toml-f/versions)


### Building from source

To build this project from the source code in this repository you need to have

- a Fortran compiler supporting Fortran 2008

  - GFortran 5 or newer
  - Intel Fortran 18 or newer
  - NAG 7 or newer

- One of the supported build systems

  - [meson](https://mesonbuild.com) version 0.55 or newer
  - [CMake](https://cmake.org/) version 3.9 or newer
  - [Fortran package manager (fpm)](https://github.com/fortran-lang/fpm) version 0.2.0 or newer

Get the source by cloning the repository

```
git clone https://github.com/toml-f/toml-f
cd toml-f
```

#### Building with meson

To integrate TOML Fortran in your meson project checkout the [Integrate with meson](https://toml-f.readthedocs.io/en/latest/how-to/integration.html#integrate-with-meson) recipe.

To build this project with meson a build-system backend is required, *i.e.* [ninja](https://ninja-build.org) version 1.7 or newer.
Setup a build with

```
meson setup _build --prefix=/path/to/install
```

You can select the Fortran compiler by the `FC` environment variable.
To compile the project run

```
meson compile -C _build
```

We employ a [validator suite](https://github.com/BurntSushi/toml-test) to test the standard compliance of this implementation.
To use this testing a `go` installation is required.
The installation of the validator suite will be handled by meson automatically without installing into the users `go` workspace.
Run the tests with

```
meson test -C _build --print-errorlogs
```

The binary used for transcribing the TOML documents to the testing format is `_build/test/toml2json` and can be used to check on per test basis.

Finally, you can install TOML Fortran using

```
meson install -C _build
```



### Building with CMake

To integrate TOML Fortran in your CMake project checkout the [Integrate with CMake](https://toml-f.readthedocs.io/en/latest/how-to/integration.html#integrate-with-cmake) recipe.

While meson is the preferred way to build this project it also offers CMake support.
Configure the CMake build with

```
cmake -B _build -G Ninja -DCMAKE_INSTALL_PREFIX=/path/to/install
```

Similar to meson the compiler can be selected with the `FC` environment variable.
You can build the project using

```
cmake --build _build
```

You can run basic unit tests using

```
ctest --test-dir _build
```

The validation suite is currently not supported as unit test for CMake builds and requires a manual setup instead using the `toml2json` binary.

Finally, you can install TOML Fortran using

```
cmake --install _build
```


### Building with fpm

To integrate TOML Fortran in your fpm project checkout the [Using the Fortran package manager](https://toml-f.readthedocs.io/en/latest/how-to/integration.html#using-the-fortran-package-manager) recipe.

The Fortran package manager ([fpm](https://github.com/fortran-lang/fpm)) supports the addition of TOML Fortran as a dependency.
In the package manifest, `fpm.toml`, you can add TOML Fortran dependency via:

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

The user documentation is available at [readthedocs](https://toml-f.readthedocs.io).
Additionally, the [FORD](https://github.com/Fortran-FOSS-Programmers/ford) generated API documentation is available [here](https://toml-f.github.io/toml-f).

To build the user documentation locally we use sphinx, install the dependencies you can use the *mamba* package manager

```
mamba create -n sphinx --file doc/requirements.txt
mamba activate sphinx
```

The documentation is build with

```
sphinx-build doc _doc
```

You can inspect the generated documentation by starting a webserver

```
python3 -m http.server -d _doc
```

And open the down URL in a browser.


### Translating the documentation

The documentation of TOML Fortran can be fully translated.
Before adding a translation, reach out to the repository maintainers by creating and issue or starting a discussion thread.

To start a new translation you need the `sphinx-intl` package which can be installed with *mamba*

```
mamba install -n sphinx sphinx-intl
```

To add a new language to the translation extract the text with `sphinx-build` and create the respective locales with `sphinx-intl` using the commands shown below.

```
sphinx-build -b gettext doc _gettext
sphinx-intl update -l en -p _gettext -d doc/locales
```

Replace the argument to the language flag `-l` with your target language, the language keys are listed [here](https://www.sphinx-doc.org/en/master/usage/configuration.html#confval-language).
The same workflow can be used for updating existing locales.
The translation files are available in `doc/locales` and can be translated using a translation-editor, like [gtranslator](https://wiki.gnome.org/Apps/Gtranslator) or [poedit](https://poedit.net/).

After a new translation is merged, a maintainer will create a new translation for the readthedocs to ensure it shows up at the pages.


### Generating the API docs

The API documentation is generated with [FORD](https://github.com/Fortran-FOSS-Programmers/ford).
We are looking for a better tool to automatically extract the documentation, suggestions and help with this effort are welcome.

The required programs can be installed with *mamba*

```
mamba create -n ford ford
mamba activate ford
```

To generate the pages use

```
ford docs.md -o _ford
```

You can inspect the generated documentation by starting a webserver

```
python3 -m http.server -d _ford
```


## Usage

To make use this library use the `tomlf` module in your projects.
You can access the individual modules but those are not considered part of the public API and might change between versions.

An example program to load and dump a TOML file would look like this:

```fortran
use tomlf
implicit none
character(len=*), parameter :: nl = new_line("a")
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

call toml_loads(table, input_string)
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

For more details checkout the [documentation pages](https://toml-f.readthedocs.io).
If you find an error in the documentation or a part is incomplete, please [open an issue](https://github.com/toml-f/toml-f/issues) or start a [discussion thread](https://github.com/orgs/toml-f/discussions).


## Contributing

This is a volunteer open source projects and contributions are always welcome.
Please, take a moment to read the [contributing guidelines](CONTRIBUTING.md) on how to get involved in TOML-Fortran.


## License

TOML-Fortran is free software: you can redistribute it and/or modify it under the terms of the [Apache License, Version 2.0](LICENSE-Apache) or [MIT license](LICENSE-MIT) at your opinion.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an _as is_ basis, without warranties or conditions of any kind, either express or implied. See the License for the specific language governing permissions and limitations under the License.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in TOML-Fortran by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
