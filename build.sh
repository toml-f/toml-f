#!/usr/bin/env bash
# This build script is distributed with TOML-Fortran for user convenience,
# it will help you compile, test and install TOML-Fortran without bothering
# with the build system (once you have it installed).
# However, you might want to have a look at meson afterall, because it is
# really not that complicated or difficult to use.

# Quick sanity check for meson installation, we want to be sure that it is there
if [ -z "$(which meson 2> /dev/null)" ]; then
   echo 2>&1 "[ERROR]"
   echo 2>&1 "Meson build system is required but not found in PATH."
   echo 2>&1 "See https://meson-build.com for instructions to install."
   exit 1
fi

# Now enter safer shell mode
set -ex

# Set a local build directory if non is given
if [ -z "${BUILD_DIR}" ]; then
   BUILD_DIR="${PWD}/_build"
fi

# Set a local install prefix if non is given
if [ -z "${PREFIX}" ]; then
   PREFIX="${PWD}/_install"
fi

# Define necessary options for meson configuration
meson_options=(
   "--prefix=${PREFIX}"
   "--buildtype=release"
   "--libdir=lib"
   "--default-library=static"
)

# Identify build directory setup or configure build system as necessary
if [ ! -d "${BUILD_DIR}/meson" ]; then
   meson setup "${BUILD_DIR}" "${meson_options[@]}"
fi

# Compile source
meson compile -C "${BUILD_DIR}"

# Run the testsuite and print logs for all failing tests
meson test -C "${BUILD_DIR}" --no-rebuild --print-errorlogs

# Install the final working version into the prefix
meson install -C "${BUILD_DIR}" --no-rebuild
