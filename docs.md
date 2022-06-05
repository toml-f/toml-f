---
project: TOML-Fortran
summary: TOML parser implementation for data serialization and deserialization in Fortran.
favicon: ./assets/toml-f-64x64.png
project_github: https://github.com/toml-f/toml-f
project_download: https://github.com/toml-f/toml-f/releases
project_website: https://toml-f.readthedocs.io
author: Sebastian Ehlert
github: https://github.com/awvwgk
src_dir: ./src
output_dir: ./docs
exclude_dir: ./tests
media_dir: ./assets
docmark: <
predocmark: >
source: true
graph: true
sort: alpha
print_creation_date: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---

{!README.md!}
