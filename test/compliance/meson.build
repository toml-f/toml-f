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

# We will run a TOML decoder validator, preferring the environment-provided
# toml-test v2 binary when available and falling back to the vendored 1.x
# subproject otherwise.
go_exe = find_program('go', required: false)
toml_test_exe = find_program('toml-test', required: false)
if not meson.is_subproject() and (toml_test_exe.found() or go_exe.found())
  toml_test_prj = subproject('toml-test-1.1.0')
  valid_tests = toml_test_prj.get_variable('valid_tests')
  invalid_tests = toml_test_prj.get_variable('invalid_tests')

  if toml_test_exe.found()
    use_v2_toml_test = true
  else
    toml_test_exe = toml_test_prj.get_variable('toml_test_exe')
    validator_testdir = toml_test_prj.get_variable('compliance_tests')
    use_v2_toml_test = false
  endif

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
    if use_v2_toml_test
      test_args = [
        'test',
        '-toml=1.1',
        '-decoder=' + toml2json_exe.full_path(),
        '-run=' + t,
      ]
    else
      test_args = ['-testdir', validator_testdir, '-run', t, '--', toml2json_exe]
    endif

    if not valid_failing.contains(t)
      test(
        t,
        toml_test_exe,
        args: test_args,
        suite: 'decoder',
      )
    else
      benchmark(
        t,
        toml_test_exe,
        args: test_args,
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
    if use_v2_toml_test
      test_args = [
        'test',
        '-toml=1.1',
        '-decoder=' + toml2json_exe.full_path(),
        '-run=' + t,
      ]
    else
      test_args = ['-testdir', validator_testdir, '-run', t, '--', toml2json_exe]
    endif

    if not invalid_failing.contains(t)
      test(
        t,
        toml_test_exe,
        args: test_args,
        suite: 'decoder',
      )
    else
      benchmark(
        t,
        toml_test_exe,
        args: test_args,
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
    if use_v2_toml_test
      test_args = [
        'test',
        '-toml=1.1',
        '-decoder=' + toml2json_exe.full_path(),
        '-encoder=' + json2toml_exe.full_path(),
        '-run=' + t,
      ]
    else
      test_args = ['-encoder', '-testdir', validator_testdir, '-run', t, '--', json2toml_exe]
    endif

    if not encoder_failing.contains(t)
      test(
        t,
        toml_test_exe,
        args: test_args,
        suite: 'encoder',
      )
    else
      benchmark(
        t,
        toml_test_exe,
        args: test_args,
        suite: 'encoder',
      )
    endif
  endforeach
endif
