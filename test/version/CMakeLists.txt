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

if(${CMAKE_VERSION} VERSION_GREATER "3.18")
  # Required for policy change
  cmake_policy(SET CMP0110 NEW)
endif()

# Check for consistency of the internal version number with the one defined
# in the projects meson.build
add_executable(
  ${PROJECT_NAME}-test-version
  "version.f90"
)
target_link_libraries(
  ${PROJECT_NAME}-test-version
  PRIVATE
  "${PROJECT_NAME}-lib"
)
add_test(
  "${PROJECT_NAME}/version"
  ${PROJECT_NAME}-test-version
  "${PROJECT_VERSION}"
)

# Check if the version in the fpm.toml package file matches the internal version
add_executable(
  ${PROJECT_NAME}-test-fpm
  "fpm.f90"
)
target_link_libraries(
  ${PROJECT_NAME}-test-fpm
  PRIVATE
  "${PROJECT_NAME}-lib"
)
add_test(
  "${PROJECT_NAME}/fpm"
  ${PROJECT_NAME}-test-fpm
  "${fpm-toml}"
)
