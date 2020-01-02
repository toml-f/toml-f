# This file is part of toml-f.
#
# Copyright (C) 2019-2020 Sebastian Ehlert
#
# toml-f is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# toml-f is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with toml-f.  If not, see <https://www.gnu.org/licenses/>.

project('toml-f',
        'fortran',
        version: '0.1',
        license: 'GPL3',
        meson_version: '>= 0.49')

fc = meson.get_compiler('fortran')

# remove line length constraint from gfortran
if fc.get_id() == 'gcc'
  add_project_arguments('-ffree-line-length-none', language: 'fortran')
endif

if get_option('debug')
  if fc.get_id() == 'gcc'
    add_project_arguments('-fcheck=all', language: 'fortran')
    add_project_arguments('-fbacktrace', language: 'fortran')
  elif fc.get_id() == 'intel'
    add_project_arguments(['-check', 'all'], language: 'fortran')
    add_project_arguments('-traceback', language: 'fortran')
  endif
else
  add_project_arguments('-DNDEBUG', language: 'fortran')
endif

srcs = []
srcs_fypp = []

srcs += files('src/tomlf08.f90')
srcs += files('src/tomlf08_constants.f90')
srcs_fypp += files('src/tomlf08_type.F90')
srcs += files('src/tomlf08_utils.f90')
srcs += files('src/tomlf08_ser.f90')
srcs_fypp += files('src/tomlf08_de.F90')

fypp = find_program('fypp', 'fypp.py', required: true)
foreach src: srcs_fypp
  srcs += configure_file(input: src, output: '@BASENAME@.f90',
                         command: [fypp, '-n', '@INPUT@', '@OUTPUT@'])
endforeach

tomlf_lib = library(meson.project_name(), srcs,
                    version: meson.project_version(),
                    install: true)

tomlf_inc = tomlf_lib.private_dir_include()
tomlf_dep = declare_dependency(link_with: tomlf_lib,
                               include_directories: tomlf_inc)

executable(meson.project_name(), files('src/test.f90'), dependencies: tomlf_dep)

ford = find_program('ford', 'ford.py', required: false)
if ford.found()
  run_target('docs', command: [ford, '-o', meson.current_build_dir()/'docs',
                               '-r', meson.project_version(), 'docs.md'])
endif

subdir('testing')
