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
    'json_de.f90',
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

  valid_failing = [
    'valid/array/array',
    'valid/array/bool',
    'valid/array/empty',
    'valid/array/hetergeneous',
    'valid/array/mixed-int-array',
    'valid/array/mixed-int-float',
    'valid/array/mixed-int-string',
    'valid/array/mixed-string-table',
    'valid/array/nested-double',
    'valid/array/nested',
    'valid/array/nospaces',
    'valid/array/string-quote-comma-2',
    'valid/array/string-quote-comma',
    'valid/array/string-with-comma',
    'valid/array/strings',
    'valid/comment/tricky',
    'valid/comment/everywhere',
    'valid/datetime/local-date',
    'valid/datetime/local-time',
    'valid/datetime/datetime',
    'valid/datetime/local',
    'valid/example',
    'valid/float/inf-and-nan',
    'valid/inline-table/nest',
    'valid/key/escapes',
    'valid/spec-example-1-compact',
    'valid/spec-example-1',
    'valid/string/escape-tricky',
    'valid/string/escapes',
    'valid/string/multiline',
    'valid/string/multiline-quotes',
    'valid/string/unicode-escape',
  ]

  foreach t : valid_tests
    if not valid_failing.contains(t)
      test(
        'decode-@0@'.format(t),
        toml_test_exe,
        args: ['-testdir', validator_testdir, '-run', t, '--', toml2json_exe],
      )
    else
      benchmark(
        'decode-@0@'.format(t),
        toml_test_exe,
        args: ['-testdir', validator_testdir, '-run', t, '--', toml2json_exe],
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
    'invalid/control/multi-del',
    'invalid/control/multi-lf',
    'invalid/control/multi-null',
    'invalid/control/multi-us',
    'invalid/control/rawmulti-del',
    'invalid/control/rawmulti-lf',
    'invalid/control/rawmulti-null',
    'invalid/control/rawmulti-us',
    'invalid/control/rawstring-del',
    'invalid/control/rawstring-lf',
    'invalid/control/rawstring-null',
    'invalid/control/rawstring-us',
    'invalid/control/string-bs',
    'invalid/control/string-del',
    'invalid/control/string-lf',
    'invalid/control/string-null',
    'invalid/control/string-us',
    'invalid/datetime/hour-over',
    'invalid/datetime/mday-over',
    'invalid/datetime/mday-under',
    'invalid/datetime/minute-over',
    'invalid/datetime/month-over',
    'invalid/datetime/month-under',
    'invalid/datetime/second-over',
    'invalid/encoding/bad-utf8-in-comment',
    'invalid/encoding/bad-utf8-in-string',
    'invalid/float/double-point-1',
    'invalid/float/double-point-2',
    'invalid/float/exp-double-e-1',
    'invalid/float/exp-double-e-2',
    'invalid/float/exp-leading-us',
    'invalid/float/exp-point-2',
    'invalid/float/trailing-us-exp',
    'invalid/inline-table/add',
    'invalid/inline-table/trailing-comma',
    'invalid/integer/incomplete-bin',
    'invalid/integer/incomplete-oct',
    'invalid/integer/incomplete-hex',
    'invalid/integer/negative-bin',
    'invalid/integer/negative-hex',
    'invalid/integer/positive-bin',
    'invalid/integer/positive-hex',
    'invalid/string/bad-codepoint',
    'invalid/string/basic-multiline-out-of-range-unicode-escape-1',
    'invalid/string/basic-multiline-out-of-range-unicode-escape-2',
    'invalid/string/basic-out-of-range-unicode-escape-1',
    'invalid/string/basic-out-of-range-unicode-escape-2',
    'invalid/table/append-with-dotted-keys-1',
    'invalid/table/append-with-dotted-keys-2',
    'invalid/array/no-close-2',
    'invalid/array/no-close',
  ]

  foreach t : invalid_tests
    if not invalid_failing.contains(t)
      test(
        'decode-@0@'.format(t),
        toml_test_exe,
        args: ['-testdir', validator_testdir, '-run', t, '--', toml2json_exe],
      )
    else
      benchmark(
        'decode-@0@'.format(t),
        toml_test_exe,
        args: ['-testdir', validator_testdir, '-run', t, '--', toml2json_exe],
      )
    endif
  endforeach

  encoder_failing = [
    'valid/array/mixed-string-table',
    'valid/comment/everywhere',
    'valid/datetime/local-date',
    'valid/datetime/local-time',
    'valid/datetime/local',
    'valid/float/inf-and-nan',
    'valid/inline-table/nest',
    'valid/key/escapes',
    'valid/key/special-chars',
    'valid/string/escapes',
    'valid/string/multiline-quotes',
  ]

  foreach t : valid_tests
    if not encoder_failing.contains(t)
      test(
        'encode-@0@'.format(t),
        toml_test_exe,
        args: ['-encoder', '-testdir', validator_testdir, '-run', t, '--', json2toml_exe],
      )
    else
      benchmark(
        'encode-@0@'.format(t),
        toml_test_exe,
        args: ['-encoder', '-testdir', validator_testdir, '-run', t, '--', json2toml_exe],
      )
    endif
  endforeach
endif
