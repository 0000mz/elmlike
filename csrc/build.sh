#!/bin/sh

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

cmake -B build -S . \
  -DCMAKE_TOOLCHAIN_FILE="${VCPKG_TOOLCHAIN_FILE}" \
  -G Ninja
