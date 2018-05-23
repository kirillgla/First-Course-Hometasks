cmake_minimum_required(VERSION 3.8)
project(task9)

set(CMAKE_C_STANDARD 99)

set(SOURCE_FILES main.c myMemoryManager.c myMemoryManager.h)
add_executable(task9 ${SOURCE_FILES})