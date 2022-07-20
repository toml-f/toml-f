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

if(NOT TARGET "test-drive::test-drive")
  find_package("test-drive")
endif()

set(
  test-srcs
  "build.f90"
  "lexer.f90"
  "main.f90"
  "parser.f90"
  "sort.f90"
  "utils.f90"
)

add_executable(
  tftester
  "${test-srcs}"
)
target_link_libraries(
  tftester
  PRIVATE
  "${PROJECT_NAME}-lib"
  "test-drive::test-drive"
)
add_test(
  "tftest"
  tftester
)
