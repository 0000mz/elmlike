project('elmlike', ['cpp'],
  version : '0.1',
  default_options : ['warning_level=3', 'cpp_std=c++20'])

# --- Dependencies ---

# Pkg-config dependencies from vcpkg
# Note: User must set PKG_CONFIG_PATH to point to vcpkg's pkgconfig dir
vulkan_dep = dependency('vulkan')
gmp_dep = dependency('gmp')
libffi_dep = dependency('libffi')
zlib_dep = dependency('zlib')
libpng_dep = dependency('libpng')
libjpeg_dep = dependency('libjpeg')
freetype_dep = dependency('freetype2')
harfbuzz_dep = dependency('harfbuzz')
icu_dep = dependency('icu-uc')
libwebp_dep = dependency('libwebp')
libwebpdemux_dep = dependency('libwebpdemux')
gtest_dep = dependency('gtest')
gtest_main_dep = dependency('gtest_main')

# Abseil dependencies (need to link specific components used)
absl_base_dep = dependency('absl_base')
absl_sync_dep = dependency('absl_synchronization')
absl_strings_dep = dependency('absl_strings')
absl_deps = [absl_base_dep, absl_sync_dep, absl_strings_dep]

# Manual Static Libraries (No pkg-config found)
# We assume vcpkg structure is consistent with what we explored
vcpkg_root = meson.current_source_dir() / 'csrc/vcpkg_installed/arm64-osx'
vcpkg_lib_dir = vcpkg_root / 'lib'
vcpkg_include_dir = vcpkg_root / 'include'

compiler = meson.get_compiler('cpp')

# Skia
skia_lib = compiler.find_library('skia', dirs : vcpkg_lib_dir, static : true)
skia_dep = declare_dependency(
  dependencies : skia_lib,
  include_directories : [
    include_directories('csrc/vcpkg_installed/arm64-osx/include'),
    include_directories('csrc/vcpkg_installed/arm64-osx/include/skia')
  ]
)

# Vk-Bootstrap
vk_bootstrap_lib = compiler.find_library('vk-bootstrap', dirs : vcpkg_lib_dir, static : true)
vk_bootstrap_dep = declare_dependency(
  dependencies : vk_bootstrap_lib,
  include_directories : include_directories('csrc/vcpkg_installed/arm64-osx/include') # It might be under include directly or include/vk-bootstrap, handled by include path
)


# Frameworks (macOS)
frameworks_dep = dependency('appleframeworks', modules : ['Cocoa', 'IOKit', 'CoreVideo', 'CoreText', 'CoreFoundation', 'CoreGraphics'], required : false)

# Subprojects
cmake = import('cmake')

# SDL3 (CMake subproject)
sdl3_proj = cmake.subproject('sdl3')
sdl3_dep = sdl3_proj.dependency('SDL3-shared') 

# GLFW (CMake subproject)
glfw_proj = cmake.subproject('glfw')
glfw_dep = glfw_proj.dependency('glfw')


# --- C++ Targets ---

inc_dirs = include_directories('.', 'csrc')

# Renderer Library (Static)
renderer_lib = static_library('renderer',
  'csrc/renderer.cc',
  include_directories : inc_dirs,
  dependencies : [skia_dep, glfw_dep, vk_bootstrap_dep, vulkan_dep, absl_deps, 
                  zlib_dep, libpng_dep, libjpeg_dep, freetype_dep, harfbuzz_dep, icu_dep,
                  libwebp_dep, libwebpdemux_dep, frameworks_dep],
)

renderer_dep = declare_dependency(
  link_with : renderer_lib,
  include_directories : inc_dirs,
  dependencies: [skia_dep, glfw_dep, vk_bootstrap_dep, vulkan_dep] # Public deps needed for headers?
)

# ElmLike Library (Shared)
# We build as shared so it links all static deps (Skia, etc) into one dynamic lib for Haskell to load.
elmlike_lib = shared_library('elmlike',
  'csrc/elmlike.cc',
  include_directories : inc_dirs,
  dependencies : [renderer_dep, sdl3_dep, absl_deps],
  cpp_args : ['-DBUILD_WITH_HASKELL_STUB=0'],
  install : true
)

elmlike_dep = declare_dependency(
  link_with : elmlike_lib,
  include_directories : inc_dirs
)

# --- Haskell Targets (Manual via custom_target) ---
# Meson haskell module is missing/broken in this env, using GHC directly.

# We need the output path of the library to pass to GHC.
# For shared library, we need to ensure GHC finds it at link time and runtime.
# On macOS, using @rpath or absolute path is needed.

# We'll compile the Haskell Main (test/Main.hs) and link against elmlike.
# Assuming test/Main.hs is the entry point user cares about for "my haskell program".
# Or maybe src/ElmLike.hs is just a lib.
# The user said "build my haskell program". `test/Main.hs` seems to be the executable entry.

# Build target for the Haskell executable
elmlike_hs = custom_target('elmlike-hs',
  output : 'elmlike-prog',
  input : ['test/Main.hs', 'src/ElmLike.hs'], # Depend on source
  depends : [elmlike_lib], # Ensure C++ lib is built first
  command : [
    'ghc',
    '-o', '@OUTPUT@',
    '-odir', '@PRIVATE_DIR@',
    '-hidir', '@PRIVATE_DIR@',
    '-stubdir', '@PRIVATE_DIR@',
    '-i' + meson.project_source_root() / 'src', # Include src dir for ElmLike module
    meson.project_source_root() / 'test/Main.hs',
    '-L' + meson.current_build_dir(),
    '-lelmlike',
    '-optl-Wl,-rpath,' + meson.current_build_dir() # Set RPATH so it finds dylib in build dir
  ],
  install : false,
  build_by_default : true
)

# 1. Compile Haskell (and generate stub)
# We need to output the object and the stub.
# Note: output filenames must match what GHC produces.
elmlike_hs_obj = custom_target('elmlike_hs_obj',
  output : ['ElmLike.o', 'ElmLike_stub.h', 'ElmLike.hi'],
  input : 'src/ElmLike.hs',
  command : [
    'ghc',
    '-c', '@INPUT@',
    '-o', '@OUTPUT0@',      # ElmLike.o
    '-odir', '@OUTDIR@',
    '-hidir', '@OUTDIR@',
    '-stubdir', '@OUTDIR@',
    '-i' + meson.project_source_root() / 'src'
  ],
  install : false,
  build_by_default : false
)

# 2. Link C++ Main with Haskell Object
elmlike_main = custom_target('elmlike-main',
  output : 'elmlike-main',
  input : ['csrc/elmlike_main.cc', elmlike_hs_obj[0], elmlike_hs_obj[1]], # cc, .o, .h
  depends : [elmlike_lib],
  command : [
    'ghc',
    '-o', '@OUTPUT@',
    '-odir', '@PRIVATE_DIR@',
    '-hidir', '@PRIVATE_DIR@',
    '-stubdir', '@PRIVATE_DIR@',
    '-no-hs-main',
    '-optcxx-std=c++20',
    '-I' + meson.project_source_root() / 'src',
    '-I' + meson.project_source_root() / 'csrc',
    '-I' + meson.current_build_dir(), # Stub is in build root
    '-optcxx-I' + meson.current_build_dir(),
    '@INPUT0@', # C++ source
    '@INPUT1@', # Haskell object
    '-L' + meson.current_build_dir(),
    '-lelmlike',
    '-optl-Wl,-rpath,' + meson.current_build_dir(),
    '-lstdc++'
  ],
  install : false,
  build_by_default : true
)

# --- Tests ---

gui_test = executable('gui_test',
  'csrc/tests/gui_test.cc',
  include_directories : inc_dirs,
  dependencies : [elmlike_dep, gtest_dep, gtest_main_dep],
  cpp_args : ['-DBUILD_WITH_HASKELL_STUB=0']
)

test('gui_test', gui_test)
