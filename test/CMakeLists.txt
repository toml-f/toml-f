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

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

# Check for consistency of the internal version number with the one defined
# in the projects meson.build
add_executable(
  tftest-version
  "${dir}/version.f90"
)
target_link_libraries(
  tftest-version
  PRIVATE
  "${PROJECT_NAME}-lib"
)
add_test(
  "version"
  tftest-version
  "${PROJECT_VERSION}"
)

# Check if the version in the fpm.toml package file matches the internal version
add_executable(
  tftest-fpm
  "${dir}/fpm.f90"
)
target_link_libraries(
  tftest-fpm
  PRIVATE
  "${PROJECT_NAME}-lib"
)
add_test(
  "fpm"
  tftest-fpm
  "${fpm-toml}"
)

# Unit testing
set(test-srcs "${dir}/tftest.f90")
add_subdirectory("tftest")
add_executable(
  tftester
  "${test-srcs}"
)
target_link_libraries(
  tftester
  PRIVATE
  "${PROJECT_NAME}-lib"
)
add_test(
  "tftest"
  tftester
)

# Serializer from TOML to a specific JSON format suitable for the validator suite
add_executable(
  toml2json
  "${dir}/json_ser.f90"
  "${dir}/toml2json.f90"
)
target_link_libraries(
  toml2json
  PRIVATE
  "${PROJECT_NAME}-lib"
)

# Deserializer for a specific JSON format suitable for the validator suite
add_executable(
  json2toml
  "${dir}/json_de.f90"
  "${dir}/json2toml.f90"
)
target_link_libraries(
  json2toml
  PRIVATE
  "${PROJECT_NAME}-lib"
)
