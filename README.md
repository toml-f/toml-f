# TOML parser for Fortran projects

[![Build Status](https://travis-ci.com/awvwgk/toml-f.svg?branch=master)](https://travis-ci.com/awvwgk/toml-f)
[![Build Status](https://github.com/awvwgk/toml-f/workflows/CI/badge.svg)](https://github.com/awvwgk/toml-f/actions)

A TOML parser implementation for data serialization and deserialization in Fortran.

- the [TOML standard](https://github.com/toml-lang/toml)
- currently supported [TOML v0.5 specification](https://github.com/toml-lang/toml/blob/v0.5.0/README.md)

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

## License

`toml-f` is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

`toml-f` is distributed in the hope that it will be useful,
but without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  See the
GNU General Public License for more details.
