# CMake configuration for square
# Author: Robin Lin
# This is a simple library used to calculate square of a double number

cmake_minimum_required(VERSION 3.12 FATAL_ERROR)
project(square)

option(BUILD_STATIC_SQUARE "Build static library for square library" ON)
option(BUILD_SHARED_SQUARE "Build shared library for square library" ON)

add_library(square square.cpp)