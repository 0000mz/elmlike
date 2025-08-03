cmake_minimum_required(VERSION 3.15)
project(elmlike)

include(FetchContent)

FetchContent_Declare(
  SDL3
  GIT_REPOSITORY https://github.com/libsdl-org/SDL.git
  GIT_TAG main
)
FetchContent_MakeAvailable(SDL3)

find_package(unofficial-skia REQUIRED)

add_library(elmlike SHARED elmlike.cc)
target_include_directories(elmlike PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(elmlike PRIVATE SDL3::SDL3 unofficial::skia::skia)
