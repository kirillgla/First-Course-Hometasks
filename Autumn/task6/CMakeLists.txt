cmake_minimum_required(VERSION 3.8)
project(task6)

set(CMAKE_C_STANDARD 99)
set(SOURCE_FILES main.c mstring.c mstring.h mappedFile.c mappedFile.h lineEnd.h)

set(MMAN_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/mman-win32-master)

add_subdirectory(mman-win32-master)
include_directories(${MMAN_INCLUDE_DIR})

add_executable(task6 ${SOURCE_FILES})
target_link_libraries(task6 mman)
ADD_DEPENDENCIES(task6 mman)
