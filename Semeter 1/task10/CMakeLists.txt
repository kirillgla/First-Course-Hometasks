cmake_minimum_required(VERSION 3.8)
project(task10)

set(CMAKE_C_STANDARD 99)

set(SOURCE_FILES main.c util.c util.h)
add_executable(task10 ${SOURCE_FILES})
