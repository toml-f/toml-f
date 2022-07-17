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
