cmake_minimum_required(VERSION 3.10)
project(elmlike)

add_library(elmlike SHARED elmlike.cc)

target_include_directories(elmlike PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
