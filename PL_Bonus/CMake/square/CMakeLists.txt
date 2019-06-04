# CMake configuration for square
# Author: Robin Lin
# This is a simple library used to calculate square of a double number

cmake_minimum_required(VERSION 3.12 FATAL_ERROR)

option(BUILD_STATIC_SQUARE "Build static library for square library" ON)
option(BUILD_SHARED_SQUARE "Build shared library for square library" OFF)

if(BUILD_STATIC_SQUARE)
	add_library(square STATIC square.cpp square.h)
endif()

if(BUILD_SHARED_SQUARE)
	set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
	add_library(square SHARED square.cpp square.h)
endif()


