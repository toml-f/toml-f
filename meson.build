# This file is part of toml-f.
#
# Copyright (C) 2019-2020 Sebastian Ehlert
#
# Licensed under either of Apache License, Version 2.0 or MIT license
# at your option; you may not use this file except in compliance with
# the License.
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

project(
  'toml-f',
  'fortran',
  version: '0.2.0',
  license: ['MIT', 'Apache-2.0'],
  meson_version: '>=0.49',
  default_options: [
    'buildtype=debugoptimized',
    'default_library=both',
  ]
)
install = not meson.is_subproject()

# General configuration information
subdir('config')

# Collect source of the project
srcs = []
subdir('src')

# TOML-Fortran library target
tomlf_lib = library(
  meson.project_name(),
  sources: srcs,
  version: meson.project_version(),
  install: install,
)

# Export dependency for other projects and test suite
tomlf_inc = tomlf_lib.private_dir_include()
tomlf_dep = declare_dependency(
  link_with: tomlf_lib,
  include_directories: tomlf_inc,
)

if install
  pkg = import('pkgconfig')
  pkg.generate(
    tomlf_lib,
    description: 'A TOML parser implementation for data serialization and deserialization in Fortran',
  )
endif
