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

ex1_srcs = []
subdir('app')
subdir('src')

example1 = executable(
  'example',
  sources: ex1_srcs,
  dependencies: tomlf_dep,
)

if os != 'windows'
  test('example-1.1', example1, args: files('package-1.toml'))
  test('example-1.2', example1, args: files('package-2.toml'))
endif
