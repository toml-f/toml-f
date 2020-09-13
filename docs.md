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
project_github: https://github.com/toml-f/toml-f
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

* the [TOML standard](https://toml.io)
* currently supported [TOML v1.0.0-rc2 specification](https://toml.io/en/v1.0.0-rc.2)


## License

TOML-Fortran is free software: you can redistribute it and/or modify it under the terms of the Apache License, Version 2.0 or MIT license at your opinion.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an _as is_ basis, without warranties or conditions of any kind, either express or implied. See the License for the specific language governing permissions and limitations under the License.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in TOML-Fortran by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
