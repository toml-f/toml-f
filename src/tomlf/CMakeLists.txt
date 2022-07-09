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

add_subdirectory("build")
add_subdirectory("de")
add_subdirectory("structure")
add_subdirectory("type")
add_subdirectory("utils")

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/all.f90"
  "${dir}/build.f90"
  "${dir}/constants.f90"
  "${dir}/datetime.f90"
  "${dir}/de.f90"
  "${dir}/diagnostic.f90"
  "${dir}/error.f90"
  "${dir}/ser.f90"
  "${dir}/structure.f90"
  "${dir}/terminal.f90"
  "${dir}/type.f90"
  "${dir}/utils.f90"
  "${dir}/version.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)
