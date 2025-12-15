#!/bin/bash
set -e

# Set source directory
SRC_DIR=$(dirname "$0")
cd "$SRC_DIR"

# Ensure vcpkg dependencies are installed
if command -v vcpkg &> /dev/null; then
    echo "Running vcpkg install..."
    (cd csrc && vcpkg install)
else
    echo "Warning: vcpkg not found in PATH. Assuming dependencies are already installed."
fi

# Path to vcpkg pkgconfig
PKG_CONFIG_PATH="$(pwd)/csrc/vcpkg_installed/arm64-osx/lib/pkgconfig"
export PKG_CONFIG_PATH

echo "Using PKG_CONFIG_PATH: $PKG_CONFIG_PATH"


# Set compilers to Clang
export CXX=clang++
export CC=clang

# Setup build directory
if [ ! -d "build" ]; then
    meson setup build
else
    # Reconfigure to ensure env changes (like CXX) are picked up
    meson setup --reconfigure build
fi

# Compile
meson compile -C build
