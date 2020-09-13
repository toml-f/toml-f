---
project: TOML-Fortran
summary: A TOML parser implementation for data serialization and deserialization in Fortran.
favicon: ./assets/toml-f-64x64.png
project_github: https://github.com/toml-f/toml-f
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

[![License](https://img.shields.io/badge/license-MIT%7CApache%202.0-blue)](https://github.com/toml-f/toml-f/blob/master/LICENSE-Apache)
[![Build](https://travis-ci.com/toml-f/toml-f.svg?branch=master)](https://travis-ci.com/toml-f/toml-f)
[![Build](https://github.com/toml-f/toml-f/workflows/CI/badge.svg)](https://github.com/toml-f/toml-f/actions)
[![docs](https://github.com/toml-f/toml-f/workflows/docs/badge.svg)](https://toml-f.github.io/toml-f)
[![codecov](https://codecov.io/gh/toml-f/toml-f/branch/master/graph/badge.svg)](https://codecov.io/gh/toml-f/toml-f)

A TOML parser implementation for data serialization and deserialization in Fortran.

* the [TOML standard](https://toml.io)
* currently supported [TOML v1.0.0-rc2 specification](https://toml.io/en/v1.0.0-rc.2)
* the [TOML-Fortran project](https://github.com/toml-f/toml-f)


## Getting Started
[![template](https://img.shields.io/badge/template-meson-success)](https://github.com/toml-f/tf-meson-example)

Create a new meson project and include `toml-f` either as git-submodule in your subprojects directory or create a wrap file to fetch it from upstream:

```ini
[wrap-git]
directory = toml-f
url = https://github.com/toml-f/toml-f
revision = head
```

To load the project the necessary boilerplate code for subprojects is just

<!--pygments doesn't know about meson, python highlighting looks okayish-->
```python
tomlf_prj = subproject(
  'toml-f',
  version: '>=0.2',
  default_options: [
    'default_library=static',
  ],
)
tomlf_dep = tomlf_prj.get_variable('tomlf_dep')
```

Now you can add `tomlf_dep` to your dependencies and access the public API by the `tomlf` module.

We recommend to set the default library type of `toml-f` to static when linking your applications or library against it.
Note for library type both and shared `toml-f` will install itself along with your project.

For more fine-tuned control you can access:
- the library target with `tomlf_lib`
- the private include dir of this target, containing the Fortran module files, with `tomlf_inc`
- the license files of `toml-f` with `tomlf_lic`

If you are linking your application statically against `toml-f` and still want to distribute the license files of `toml-f` (thank you), just use

```python
install_data(
  tomlf_prj.get_variable('tomlf_lic'),
  install_dir: get_option('datadir')/'licenses'/meson.project_name()/'toml-f',
)
```
