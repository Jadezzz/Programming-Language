cmake_minimum_required(VERSION 3.4)

project(NANATest)

file(GLOB SOURCE src/*.cpp)
file(GLOB HEADER src/*.h)


set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")

set(NANA_PATH "${CMAKE_CURRENT_LIST_DIR}/ext/nana" CACHE PATH "")
set(FMT_PATH "${CMAKE_CURRENT_LIST_DIR}/ext/fmt" CACHE PATH "")
set(SQUARE_PATH "${CMAKE_CURRENT_LIST_DIR}/square" CACHE PATH "")

if (NOT EXISTS "${NANA_PATH}/CMakeLists.txt")
    message(FATAL_ERROR "Please specify valid nana path!")
endif ()

if (NOT EXISTS "${FMT_PATH}/CMakeLists.txt")
    message(FATAL_ERROR "Please specify valid fmt path!")
endif ()

if (NOT EXISTS "${SQUARE_PATH}/CMakeLists.txt")
    message(FATAL_ERROR "Please specify valid square path!")
endif ()

include_directories("${SQUARE_PATH}")

add_subdirectory(${NANA_PATH})
add_subdirectory(${FMT_PATH})
add_subdirectory(${SQUARE_PATH})

add_executable(NANATest ${SOURCE} ${HEADER})
target_link_libraries(NANATest nana)
target_link_libraries(NANATest fmt)
target_link_libraries(NANATest square)
