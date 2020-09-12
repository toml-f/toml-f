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

test(
  'version',
  executable('tftest-version', files('version.f90'), dependencies: tomlf_dep),
  args: meson.project_version(),
)

if os != 'windows'
  test(
    'fpm',
    executable('tftest-fpm', files('fpm.f90'), dependencies: tomlf_dep),
    args: fpm_toml,
  )
endif

toml2json_exe = executable(
  'toml2json',
  sources: files(
    'json.f90',
    'toml2json.f90',
  ),
  dependencies: tomlf_dep,
)

# We will run a TOML decoder validator, in case we find a go installation
go_exe = find_program('go', required: false)
if not meson.is_subproject() and go_exe.found()
  # Obviously, we don't want to write into the users go-workspace, therefore,
  # we will create our own local workspace by overwriting the GOPATH variable
  validator_env = environment({
    'GOPATH': meson.current_build_dir(),
  })

  compliance_prj = subproject('toml-tests')
  compliance_testdir = compliance_prj.get_variable('compliance_tests')

  # Explicitly specify the validator test directories
  validator_testdir = meson.current_build_dir()/'src'/'github.com'/'toml-f'/'toml-test'/'tests'

  # First, we need to fetch the validator tests from upstream
  get_toml_test = run_command(
    go_exe,
    'get',
    'github.com/toml-f/toml-test',
    env: validator_env,
  )

  # To make sure the command is actually used before we declare the test we
  # make it dependent on the success of the go get command
  if get_toml_test.returncode() == 0
    # Finally, create the decoder test
    # The decoder test contains the tests from the BurntSushi/toml-test suite
    # and the following changes
    # - the TOML 0.4 specific mixed type arrays tests are removed
    # - the Unicode specific tests are removed, since toml-f doesn't supports them
    # - additional tests for TOML 0.5 and TOML 1.0 specific features are added
    # - additional coverage tests are added
    test(
      'decoder',
      find_program('toml-test', dirs: meson.current_build_dir()/'bin'),
      args: ['-testdir', compliance_testdir, toml2json_exe],
      env: validator_env,
    )
    # This is the original BurntSushi/toml-test suite
    # -> this will currently fail due to the missing Unicode support in toml-f
    benchmark(
      'decoder',
      find_program('toml-test', dirs: meson.current_build_dir()/'bin'),
      args: ['-testdir', validator_testdir, toml2json_exe],
      env: validator_env,
    )
  endif
endif

# Additional examples are compiled and tested to keep them working
subdir('example-1')
