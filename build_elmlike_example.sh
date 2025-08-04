#!/bin/sh
# Build an executable elmlike example application.
#
# INFO: To build an elmlike example, the haskell program has to be built
# into a library first. The library is exported to a C header and archive.
# That archive needs to be available to the runtime to execute the program.

function fail {
  echo "ERROR: ${1}"
  exit 1
}

function dir_exists {
  [ -d "${1}" ]
}

function is_not_empty {
  [ -n "${1}" ]
}

echo "> Building haskell program"
cabal build || fail "Build failed..."

# Within dist-newstyle/build/<arch>/<ghc-version>/<app-version>/build/...
# there is an archive that is generated called libHSelmlike-<version>-inplace.a,
# along with a ElmLike_stub.h in the same directory.
# Those need to be copied to csrc/gen
mkdir -p csrc/gen

ELMLIKE_HS_LIB_HEADER=$(find "dist-newstyle" -name "*ElmLike_stub.h")
ELMLIKE_HS_LIB_ARCHIVE=$(find "dist-newstyle" -name "*elmlike*inplace.a")

is_not_empty "${ELMLIKE_HS_LIB_HEADER}" || fail "Could not find stub header."
is_not_empty "${ELMLIKE_HS_LIB_ARCHIVE}" || fial "Could not find stub archive."

echo "elmlike stub header: ${ELMLIKE_HS_LIB_HEADER}"
echo "elmlike stub archive: ${ELMLIKE_HS_LIB_ARCHIVE}"

ELMLIKE_LIB_FULLPATH="$(pwd)/$(dirname ${ELMLIKE_HS_LIB_HEADER})"

# Also need to find the filepath for HsFFI.h. This is provided by ghc
# within the library dir: `ghc --print-libdir`
HSFFI_HEADER="$(find "$(ghc --print-libdir)" -name "HsFFI.h" -type f 2>/dev/null)"
is_not_empty "${HSFFI_HEADER}" || fail "Failed to find HsFFI.h in GHC lib."

# Some extra GHC archives are needed...
# Just going to collect all of the archives in the ghc libdir and hope it
# works, lol.
GHC_EXTRA_LDFLAGS=""
GHC_EXTRA_CFLAGS=""

# Loops through all archives and for each archive, outputs the line in this form:
# > <archive_dir> <libname>
LIBDIR_AND_ARCHIVE=$(find "$(ghc --print-libdir)" -type f -name "*.a" -exec bash -c '
function extract_ghc_flags_from_file {
  lib_path="${1}"
  lib_file="$(basename "${lib_path}" .a)"
  lib_dir="$(dirname ${lib_path})"
  lib_name="$(echo "${lib_file}" | sed "s/^lib//")"

  echo "${lib_dir} ${lib_name}"
}
extract_ghc_flags_from_file "${0}"
' {} \;)

while IFS= read -r line; do
  archive_dir="$(echo ${line} | awk '{ print $1 }')"
  lib="$(echo ${line} | awk '{ print $2 }')"
  GHC_EXTRA_LDFLAGS="${GHC_EXTRA_LDFLAGS} -L${archive_dir} -l${lib}"
done <<< "${LIBDIR_AND_ARCHIVE}"

cd csrc
ELMLIKE_HS_STUB_DIR="${ELMLIKE_LIB_FULLPATH}" \
  ELMLIKE_ARCHIVE_NAME="$(basename ${ELMLIKE_HS_LIB_ARCHIVE})" \
  HSFFI_INCLUDE_DIR="$(dirname ${HSFFI_HEADER})" \
  GHC_EXTRA_CFLAGS="" \
  GHC_EXTRA_LDFLAGS="${GHC_EXTRA_LDFLAGS}" \
  ./build.sh release verbose build_hs_stub=on
