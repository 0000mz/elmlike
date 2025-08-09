cmake_minimum_required(VERSION 3.15)
project(elmlike)

######################################################
### General Settings
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(GLFW_BUILD_DOCS OFF CACHE BOOL "" FORCE)
set(GLFW_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(GLFW_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)

option(BUILD_WITH_HASKELL_STUB "Build binaries that depend on haskell stub" OFF)

if (APPLE)
  enable_language(OBJC)
endif()

######################################################
### Dependencies
include(FetchContent)
FetchContent_Declare(
  SDL3
  GIT_REPOSITORY https://github.com/libsdl-org/SDL.git
  GIT_TAG main
)
FetchContent_MakeAvailable(SDL3)
FetchContent_Declare(
  glfw
  GIT_REPOSITORY https://github.com/glfw/glfw.git
  GIT_TAG "3.4"
)
FetchContent_MakeAvailable(glfw)

find_package(PkgConfig REQUIRED)
find_package(unofficial-skia REQUIRED)
find_package(vk-bootstrap CONFIG REQUIRED)
find_package(Vulkan REQUIRED)
find_package(GTest CONFIG REQUIRED)
find_package(Iconv REQUIRED)
find_package(unofficial-libffi CONFIG REQUIRED)
pkg_check_modules(gmp REQUIRED IMPORTED_TARGET gmp)

######################################################
### ElmLike C Lib
set(ELMLIKE_LIBRARY_DEPS
    SDL3::SDL3
    unofficial::skia::skia
    glfw
    vk-bootstrap::vk-bootstrap
    vk-bootstrap::vk-bootstrap-compiler-warnings
    Vulkan::Vulkan
)

# ElmLike: Shared
add_library(elmlike SHARED elmlike.cc)
target_include_directories(elmlike PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(elmlike PRIVATE ${ELMLIKE_LIBRARY_DEPS})
if(APPLE)
  target_link_libraries(elmlike PUBLIC "-framework Cocoa")
endif()

# ElmLike: Static
add_library(elmlike_static STATIC elmlike.cc)
target_include_directories(elmlike_static PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(elmlike_static PRIVATE ${ELMLIKE_LIBRARY_DEPS})
if(APPLE)
  target_link_libraries(elmlike_static PUBLIC "-framework Cocoa")
endif()

if (BUILD_WITH_HASKELL_STUB)
  add_executable(elmlike_main elmlike_main.cc)
  target_link_libraries(elmlike_main PRIVATE
    elmlike_static

    # TODO: These libraries are needed by the archives linked in for ghc.
    # There's gotta be a way, maybe pkgconfig, to specify the ghc archives
    # as a required package so that the package's dependencies are also
    # resolved and do not need to be enumerated at this level.
    Iconv::Iconv
    unofficial::libffi::libffi
    PkgConfig::gmp)
endif()

######################################################
### ElmLike Tests
enable_testing()
add_executable(gui_test tests/gui_test.cc)
target_link_libraries(gui_test PRIVATE
  GTest::gtest_main
  GTest::gmock
  elmlike_static)
add_test(AllTestsInMain gui_test)

## TODO: Disable building all tests, i.e. SDL_test
