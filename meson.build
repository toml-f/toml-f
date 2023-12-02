# This file is part of toml-f.
# SPDX-Identifier: Apache-2.0 OR MIT
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
  version: '0.4.2',
  license: 'Apache-2.0 OR MIT',
  meson_version: '>=0.55',
  default_options: [
    'buildtype=debugoptimized',
    'default_library=both',
  ],
)
install = not (meson.is_subproject() and get_option('default_library') == 'static')

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

# Package the license files
tomlf_lic = files(
  'LICENSE-MIT',
  'LICENSE-Apache',
)

if install
  # Distribute the license files in share/licenses/<name>
  install_data(
    tomlf_lic,
    install_dir: get_option('datadir')/'licenses'/meson.project_name()
  )

  module_id = meson.project_name() / 'modules'
  meson.add_install_script(
    find_program(files('config'/'install-mod.py')),
    get_option('includedir') / module_id,
  )

  pkg = import('pkgconfig')
  pkg.generate(
    tomlf_lib,
    description: 'A TOML parser implementation for data serialization and deserialization in Fortran',
    subdirs: ['', module_id],
  )
endif

# add the testsuite
fpm_toml = meson.current_source_dir()/'fpm.toml'
subdir('test')
