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
add_executable(
  toml2json
  "json_ser.f90"
  "toml2json.f90"
)
target_link_libraries(
  toml2json
  PRIVATE
  "${PROJECT_NAME}-lib"
)

# Deserializer for a specific JSON format suitable for the validator suite
add_executable(
  json2toml
  "json_lexer.f90"
  "json_parser.f90"
  "json2toml.f90"
)
target_link_libraries(
  json2toml
  PRIVATE
  "${PROJECT_NAME}-lib"
)

# Run the toml-test validator suite if available
find_program(TOML_TEST_EXE toml-test)
if(TOML_TEST_EXE)
  set(
    decoder-failing
    "invalid/array/extending-table"
    "invalid/control/bare-cr"
    "invalid/control/comment-cr"
    "invalid/control/comment-del"
    "invalid/control/comment-ff"
    "invalid/control/comment-lf"
    "invalid/control/comment-null"
    "invalid/control/comment-us"
    "invalid/control/multi-cr"
    "invalid/control/rawmulti-cr"
    "invalid/encoding/bad-codepoint"
    "invalid/encoding/bad-utf8-in-comment"
    "invalid/encoding/bad-utf8-in-multiline"
    "invalid/encoding/bad-utf8-in-multiline-literal"
    "invalid/encoding/bad-utf8-in-string"
    "invalid/encoding/bad-utf8-in-string-literal"
    "invalid/string/bad-uni-esc-ml-06"
    "invalid/string/basic-multiline-out-of-range-unicode-escape-02"
    "invalid/string/basic-out-of-range-unicode-escape-02"
    "invalid/table/append-with-dotted-keys-01"
    "invalid/table/append-with-dotted-keys-02"
    "invalid/table/append-with-dotted-keys-04"
    "invalid/table/append-with-dotted-keys-08"
    "invalid/table/super-twice"
    "valid/key/quoted-unicode"
    "valid/string/multibyte-escape"
    "valid/string/quoted-unicode"
  )
  set(
    encoder-failing
    "encoder/array/mixed-string-table"
    "encoder/inline-table/end-in-bool"
    "encoder/inline-table/key-dotted-05"
    "encoder/key/quoted-unicode"
    "encoder/key/special-chars"
    "encoder/spec-1.1.0/common-17"
    "encoder/spec-1.1.0/common-23"
    "encoder/spec-1.1.0/common-35"
    "encoder/string/escape-esc"
    "encoder/string/escapes"
    "encoder/string/hex-escape"
    "encoder/string/quoted-unicode"
    "encoder/string/unicode-escape"
  )
  string(REPLACE ";" "," decoder-skip "${decoder-failing}")
  string(REPLACE ";" "," encoder-skip "${encoder-failing}")

  add_test(
    NAME "toml-test/decoder"
    COMMAND
    "${TOML_TEST_EXE}" "test" "-toml=1.1" "-color=never"
    "-decoder=$<TARGET_FILE:toml2json>"
    "-skip=${decoder-skip}"
  )
  add_test(
    NAME "toml-test/encoder"
    COMMAND
    "${TOML_TEST_EXE}" "test" "-toml=1.1" "-color=never"
    "-decoder=$<TARGET_FILE:toml2json>"
    "-encoder=$<TARGET_FILE:json2toml>"
    "-run=encoder/*" "-run=encoder/*/*"
    "-skip=${encoder-skip}"
  )
endif()
