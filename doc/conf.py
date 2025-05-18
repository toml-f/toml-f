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

import sys, os.path as op

sys.path.insert(0, op.abspath(op.join(op.dirname(__file__), "_vendor")))


project = "toml-f"
author = "Sebastian Ehlert"
copyright = f"2019-2022, {author}"

version = "0.4.2"
release = version

extensions = [
    "ablog",
    "sphinx_design",
    "sphinx_copybutton",
    "sphinx.ext.intersphinx",
    "sphinxcontrib_ansi",
]

html_theme = "sphinx_book_theme"
html_title = "TOML Fortran"
html_logo = "../assets/toml-f.svg"
html_favicon = "../assets/toml-f.svg"

_extra_navbar = """
<div class="sd-fs-4">
<a href="https://fortran-lang.discourse.group/" target="_blank">
    <i class="fab fa-discourse"></i>
</a>
<a href="https://github.com/orgs/toml-f/discussions" target="_blank">
    <i class="fa fa-comments"></i>
</a>
<a href="https://github.com/toml-f" target="_blank">
    <i class="fab fa-github"></i>
</a>
<a href="https://github.com/sponsors/awvwgk" target="_blank">
    <i class="fa fa-heart"></i>
</a>
</div>
"""

html_theme_options = {
    "repository_url": "https://github.com/toml-f/toml-f",
    "repository_branch": "main",
    "use_repository_button": True,
    "use_edit_page_button": True,
    "use_download_button": False,
    "path_to_docs": "doc",
    "extra_navbar": _extra_navbar,
}

html_css_files = ["css/custom.css"]
html_static_path = ["_static"]
templates_path = ["_templates"]
locale_dirs = ["locales"]
copybutton_selector = "div:not(.no-copybutton) > div.highlight > pre"
blog_path = "news/index"
blog_post_pattern = "news/**"

master_doc = "index"
