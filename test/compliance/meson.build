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

# Serializer from TOML to a specific JSON format suitable for the validator suite
toml2json_exe = executable(
  'toml2json',
  sources: files(
    'json_ser.f90',
    'toml2json.f90',
  ),
  dependencies: tomlf_dep,
)

# Deserializer for a specific JSON format suitable for the validator suite
json2toml_exe = executable(
  'json2toml',
  sources: files(
    'json_lexer.f90',
    'json_parser.f90',
    'json2toml.f90',
  ),
  dependencies: tomlf_dep,
)

# We will run a TOML decoder validator, in case we find a go installation
go_exe = find_program('go', required: false)
if not meson.is_subproject() and go_exe.found()
  toml_test_prj = subproject('toml-test-1.1.0')
  toml_test_exe = toml_test_prj.get_variable('toml_test_exe')
  validator_testdir = toml_test_prj.get_variable('compliance_tests')
  valid_tests = toml_test_prj.get_variable('valid_tests')
  invalid_tests = toml_test_prj.get_variable('invalid_tests')

  if not toml_test_exe.found()
    subdir_done()
  endif

  valid_failing = [
    'valid/key/escapes',
    'valid/string/escape-tricky',
    'valid/string/escapes',
    'valid/string/multiline',
  ]

  foreach t : valid_tests
    if not valid_failing.contains(t)
      test(
        t,
        toml_test_exe,
        args: ['-testdir', validator_testdir, '-run', t, '--', toml2json_exe],
        suite: 'decoder',
      )
    else
      benchmark(
        t,
        toml_test_exe,
        args: ['-testdir', validator_testdir, '-run', t, '--', toml2json_exe],
        suite: 'decoder',
      )
    endif
  endforeach

  invalid_failing = [
    'invalid/control/bare-cr',
    'invalid/control/comment-cr',
    'invalid/control/comment-del',
    'invalid/control/comment-lf',
    'invalid/control/comment-null',
    'invalid/control/comment-us',
    'invalid/encoding/bad-utf8-in-comment',
    'invalid/encoding/bad-utf8-in-string',
    'invalid/string/basic-out-of-range-unicode-escape-2',
    'invalid/table/append-with-dotted-keys-1',
    'invalid/table/append-with-dotted-keys-2',
  ]

  foreach t : invalid_tests
    if not invalid_failing.contains(t)
      test(
        t,
        toml_test_exe,
        args: ['-testdir', validator_testdir, '-run', t, '--', toml2json_exe],
        suite: 'decoder',
      )
    else
      benchmark(
        t,
        toml_test_exe,
        args: ['-testdir', validator_testdir, '-run', t, '--', toml2json_exe],
        suite: 'decoder',
      )
    endif
  endforeach

  encoder_failing = [
    'valid/array/mixed-string-table',
    'valid/inline-table/end-in-bool',
    'valid/key/escapes',
    'valid/key/special-chars',
    'valid/string/escapes',
    'valid/string/multiline-quotes',
  ]

  foreach t : valid_tests
    if not encoder_failing.contains(t)
      test(
        t,
        toml_test_exe,
        args: ['-encoder', '-testdir', validator_testdir, '-run', t, '--', json2toml_exe],
        suite: 'encoder',
      )
    else
      benchmark(
        t,
        toml_test_exe,
        args: ['-encoder', '-testdir', validator_testdir, '-run', t, '--', json2toml_exe],
        suite: 'encoder',
      )
    endif
  endforeach
endif
