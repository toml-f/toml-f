---
project: TOML-Fortran
summary: A TOML parser implementation for data serialization and deserialization in Fortran.
author: Sebastian Ehlert
license: by-sa
src_dir: ./src
output_dir: ./docs
exclude_dir: ./testing
media_dir: ./assets
favicon: ./assets/toml-f-64x64.png
preprocessor: fypp
github: https://github.com/awvwgk
project_github: https://github.com/awvwgk/toml-f
docmark: <
predocmark: >
source: true
graph: true
sort: alpha
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty

---

[TOC]

## Brief Description

A TOML parser implementation for data serialization and deserialization in Fortran.

- the [TOML standard](https://github.com/toml-lang/toml)
- currently supported [TOML v0.5 specification](https://github.com/toml-lang/toml/blob/v0.5.0/README.md)

## License

TOML-Fortran is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TOML-Fortran is distributed in the hope that it will be useful,
but without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  See the
GNU General Public License for more details.

*[API]: Application Programming Interface: a set of routines, protocols, and tools for building software applications
*[TOML]: Tom's Obvious, Minimal Language: a minimal configuration file format that's easy to read due to obvious semantics
