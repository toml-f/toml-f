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

# Check for consistency of the internal version number with the one defined
# in the projects meson.build
test(
  'version',
  executable('tftest-version', files('version.f90'), dependencies: tomlf_dep),
  args: meson.project_version(),
)

# Check if the version in the fpm.toml package file matches the internal version
test(
  'fpm',
  executable('tftest-fpm', files('fpm.f90'), dependencies: tomlf_dep),
  args: fpm_toml,
)
