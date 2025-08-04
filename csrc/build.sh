#!/bin/sh
# Build the ElmLike c libraries using cmake.
#  arg1: release|debug
#  arg2: verbose|silent

VERBOSE_MAKEFILE="OFF"
GENERATOR="Ninja"
BUILD_DIR="build"
BUILD_TYPE="Debug"

if [[ "${1}" == "release" ]]; then
  BUILD_TYPE="Release"
  BUILD_DIR="build_release"
fi

if [[ "${2}" == "verbose" ]]; then
  # Can't use Ninja to get verbose makefile output.
  VERBOSE_MAKEFILE="ON"
  GENERATOR="Unix Makefiles"
fi

echo "Build type: ${BUILD_TYPE}"
echo "Build directory: ${BUILD_DIR}"
echo ""

echo "This build requires using vcpkg."
echo "Make sure it is installed and present in your PATH."
echo "Note: It assumes the vcpkg executable is within the"
echo "cloned vcpkg directory.\n"

echo "Make sure to run \"vcpkg install skia\".\n"

function fail {
  echo "${1}"
  exit 1
}

which vcpkg > /dev/null || fail "Cannot find vcpkg"

VCPKG_EXE=$(which vcpkg)
VCPKG_DIR=$(dirname "${VCPKG_EXE}")

VCPKG_TOOLCHAIN_FILE="${VCPKG_DIR}/scripts/buildsystems/vcpkg.cmake"

cmake -B "${BUILD_DIR}" -S . \
  -DCMAKE_TOOLCHAIN_FILE="${VCPKG_TOOLCHAIN_FILE}" \
  -G "${GENERATOR}" \
  -DCMAKE_BUILD_TYPE="${BUILD_TYPE}" \
  -DCMAKE_VERBOSE_MAKEFILE="${VERBOSE_MAKEFILE}" \
  || fail "Failed to configure with cmake"
cmake --build "${BUILD_DIR}" || fail "Failed to build"
