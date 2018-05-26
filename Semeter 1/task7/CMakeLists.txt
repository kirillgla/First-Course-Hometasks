cmake_minimum_required(VERSION 3.8)
project(task7)

set(CMAKE_CXX_STANDARD 11)

set(SOURCE_FILES
        hashTable.c
        hashTable.h
        linkedList.c
        linkedList.h
        main.c
        pair.c
        pair.h)

add_executable(task7 ${SOURCE_FILES})