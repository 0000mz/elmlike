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
sdl3_opts = cmake.subproject_options()
sdl3_opts.add_cmake_defines({
  'SDL_TESTS': 'OFF',
  'SDL_EXAMPLES': 'OFF',
  'SDL_INSTALL_TESTS': 'OFF'
})
sdl3_proj = cmake.subproject('sdl3', options : sdl3_opts)
sdl3_dep = sdl3_proj.dependency('SDL3-shared') 

# GLFW (CMake subproject)
glfw_opts = cmake.subproject_options()
glfw_opts.add_cmake_defines({
  'GLFW_BUILD_DOCS': 'OFF',
  'GLFW_BUILD_TESTS': 'OFF',
  'GLFW_BUILD_EXAMPLES': 'OFF'
})
glfw_proj = cmake.subproject('glfw', options : glfw_opts)
glfw_dep = glfw_proj.dependency('glfw')


# --- C++ Targets ---

inc_dirs = include_directories('.', 'csrc')


# 1. Compile Haskell Object (Global, not variant-specific)
# We need to output the object and the stub.
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

# Retrieve GHC linker flags (libraries and paths)
ghc_info = run_command('python3', meson.current_source_dir() / 'csrc/get_ghc_info.py', check: true)
ghc_flags = ghc_info.stdout().strip().split('\n')
ghc_inc_args = []
ghc_link_args = []
foreach f : ghc_flags
  if f.startswith('-I')
    ghc_inc_args += f
  else
    ghc_link_args += f
  endif
endforeach

# Build Variants (Default, UBSan, TSan)
# Note: MSan is not supported on macOS.
# UBSan: -fno-sanitize=vptr needed because Skia (vcpkg) is likely built with -fno-rtti
build_variants = [
  {'name': '', 'opts': [], 'args': []},
  {'name': 'asan', 'opts': ['b_sanitize=address'], 'args': []},
  {'name': 'ubsan', 'opts': ['b_sanitize=undefined'], 'args': ['-fno-sanitize=vptr']},
  {'name': 'tsan', 'opts': ['b_sanitize=thread'], 'args': []}
]

elmlike_lib_default = disabler()

foreach v : build_variants
  variant = v['name']
  opts = v['opts']
  extra_args = v['args']
  
  suffix = (variant == '' ? '' : '_' + variant)
  
  # --- Renderer Library (Static) ---
  renderer_lib = static_library('renderer' + suffix,
    'csrc/renderer.cc',
    include_directories : inc_dirs,
    dependencies : [skia_dep, glfw_dep, vk_bootstrap_dep, vulkan_dep, absl_deps, 
                    zlib_dep, libpng_dep, libjpeg_dep, freetype_dep, harfbuzz_dep, icu_dep,
                    libwebp_dep, libwebpdemux_dep, frameworks_dep],
    override_options : opts, 
    cpp_args : extra_args
  )

  renderer_dep = declare_dependency(
    link_with : renderer_lib,
    include_directories : inc_dirs,
    dependencies: [skia_dep, glfw_dep, vk_bootstrap_dep, vulkan_dep]
  )

  # --- ElmLike Library (Shared) ---
  elmlike_lib = shared_library('elmlike' + suffix,
    'csrc/elmlike.cc',
    include_directories : inc_dirs,
    dependencies : [renderer_dep, sdl3_dep, absl_deps],
    install : true,
    override_options : opts,
    cpp_args : extra_args
  )

  elmlike_dep = declare_dependency(
    link_with : elmlike_lib,
    include_directories : inc_dirs
  )

  if variant == ''
    elmlike_lib_default = elmlike_lib
  endif

  # --- elmlike-main Executable ---
  executable('elmlike-main' + suffix,
    sources : ['csrc/elmlike_main.cc', elmlike_hs_obj[0]], 
    include_directories : [inc_dirs, include_directories('src')],
    dependencies : [elmlike_dep],
    cpp_args : ['-I' + meson.current_build_dir()] + ghc_inc_args + extra_args,
    link_args : ghc_link_args,
    install : false,
    build_by_default : true,
    override_options : opts
  )

  # --- gui_test Executable ---
  t = executable('gui_test' + suffix,
    'csrc/tests/gui_test.cc',
    include_directories : inc_dirs,
    dependencies : [elmlike_dep, gtest_dep, gtest_main_dep],
    override_options : opts,
    cpp_args : extra_args
  )
  
  test('gui_test' + suffix, t)
endforeach


# --- Haskell Pure Target (uses default elmlike shared lib) ---
elmlike_hs = custom_target('elmlike-hs',
  output : 'elmlike-prog',
  input : ['test/Main.hs', 'src/ElmLike.hs'], 
  depends : [elmlike_lib_default], 
  command : [
    'ghc',
    '-o', '@OUTPUT@',
    '-odir', '@PRIVATE_DIR@',
    '-hidir', '@PRIVATE_DIR@',
    '-stubdir', '@PRIVATE_DIR@',
    '-i' + meson.project_source_root() / 'src', 
    meson.project_source_root() / 'test/Main.hs',
    '-L' + meson.current_build_dir(),
    '-lelmlike',
    '-optl-Wl,-rpath,' + meson.current_build_dir() 
  ],
  install : false,
  build_by_default : true
)
