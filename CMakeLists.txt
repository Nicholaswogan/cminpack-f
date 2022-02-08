cmake_minimum_required(VERSION 3.14)

project(cminpack-f VERSION "0.1.0" LANGUAGES Fortran C)

include(cmake/CPM.cmake)

CPMAddPackage(
  NAME cminpack
  VERSION 1.3.8
  OPTIONS "CMINPACK_PRECISION d"
  GITHUB_REPOSITORY "devernay/cminpack"
  GIT_TAG "v1.3.8"
  EXCLUDE_FROM_ALL ON
)

add_library(cminpack_wrapper cminpack_wrapper.c cminpack_wrapper.f90)
target_link_libraries(cminpack_wrapper cminpack)

add_executable(test_wrapper test_wrapper.f90)
target_link_libraries(test_wrapper cminpack_wrapper)


