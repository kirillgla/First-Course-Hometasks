cmake_minimum_required(VERSION 3.8)
project(chunkedArray)

set(CMAKE_C_STANDARD 99)

set(SOURCE_FILES chunkedArray.h chunkedArray.c main.c)
add_executable(classWork ${SOURCE_FILES})