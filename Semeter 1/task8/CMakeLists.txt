cmake_minimum_required(VERSION 3.8)
project(task8)

set(CMAKE_CXX_STANDARD 11)

set(SOURCE_FILES main.c util.h util.c)

add_executable(junk ${SOURCE_FILES})