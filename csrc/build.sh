#!/bin/sh
# Build the ElmLike c libraries using cmake.
#  arg1: release|debug
#  arg2: verbose|silent
#  arg3: build_hs_stub={on,off}
#   If true, the cmake build will include the haskell stub
#   and all targets that depend on it.

VERBOSE_MAKEFILE="OFF"
GENERATOR="Ninja"
BUILD_DIR="build"
BUILD_TYPE="Debug"
BUILD_HASKELL_STUB="OFF"
CFLAGS=""
LDFLAGS=""

# TODO: This is duplicated from the other build script in this
# project. Put common functions in a utility script and source it.
function is_not_empty {
  [ -n "${1}" ]
}
function fail {
  echo "${1}"
  exit 1
}
function libname {
  lib_file="${1}"
  lib_name=$(basename "$lib_file" .a | sed 's/^lib//')
  echo "$lib_name"
}

if [[ "${1}" == "release" ]]; then
  BUILD_TYPE="Release"
  BUILD_DIR="build_release"
fi

if [[ "${2}" == "verbose" ]]; then
  # Can't use Ninja to get verbose makefile output.
  VERBOSE_MAKEFILE="ON"
  GENERATOR="Unix Makefiles"
fi

if [[ "${3}" == "build_hs_stub=on" ]]; then
  BUILD_HASKELL_STUB="ON"
  BUILD_DIR="${BUILD_DIR}_stub"

  is_not_empty "${ELMLIKE_HS_STUB_DIR}"  || fail "Stub directory not defined"
  is_not_empty "${ELMLIKE_ARCHIVE_NAME}" || fail "Stub archive not defined"
  is_not_empty "${HSFFI_INCLUDE_DIR}"    || fail "HSFFI include dir not defined"

  CFLAGS="${CFLAGS} ${GHC_EXTRA_CFLAGS} -I${ELMLIKE_HS_STUB_DIR} -L${ELMLIKE_HS_STUB_DIR} -I${HSFFI_INCLUDE_DIR}"
  LDFLAGS="${LDFLAGS} ${GHC_EXTRA_LDFLAGS} -L${ELMLIKE_HS_STUB_DIR} -l$(libname ${ELMLIKE_ARCHIVE_NAME})"

  echo "Building hs stub:"
  echo "  dir -> ${ELMLIKE_HS_STUB_DIR}"
  echo "  archive -> ${ELMLIKE_ARCHIVE_NAME}"
  echo "  hsffi inc dir -> ${HSFFI_INCLUDE_DIR}"
  echo ""
fi

echo "Build type: ${BUILD_TYPE}"
echo "Build directory: ${BUILD_DIR}"
echo ""

echo "This build requires using vcpkg."
echo "Make sure it is installed and present in your PATH."
echo "Note: It assumes the vcpkg executable is within the"
echo "cloned vcpkg directory.\n"

echo "Make sure to run \"vcpkg install skia\".\n"

which vcpkg > /dev/null || fail "Cannot find vcpkg"

VCPKG_EXE=$(which vcpkg)
VCPKG_DIR=$(dirname "${VCPKG_EXE}")

VCPKG_TOOLCHAIN_FILE="${VCPKG_DIR}/scripts/buildsystems/vcpkg.cmake"

cmake -B "${BUILD_DIR}" -S .                        \
  -DCMAKE_TOOLCHAIN_FILE="${VCPKG_TOOLCHAIN_FILE}"  \
  -G "${GENERATOR}"                                 \
  -DCMAKE_BUILD_TYPE="${BUILD_TYPE}"                \
  -DCMAKE_VERBOSE_MAKEFILE="${VERBOSE_MAKEFILE}"    \
  -DBUILD_WITH_HASKELL_STUB="${BUILD_HASKELL_STUB}" \
  -DCMAKE_CXX_FLAGS="${CFLAGS}"                     \
  -DCMAKE_EXE_LINKER_FLAGS="${LDFLAGS}"             \
  || fail "Failed to configure with cmake"
cmake --build "${BUILD_DIR}" || fail "Failed to build"
