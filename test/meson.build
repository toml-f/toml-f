# This file is part of toml-f.
#
# Copyright 2019-2020 Sebastian Ehlert
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

tests = [
  'example',
]

foreach t : tests
  test(
    t,
    executable(
      t,
      files('@0@.f90'.format(t)),
      dependencies: tomlf_dep,
    ),
  )
endforeach
