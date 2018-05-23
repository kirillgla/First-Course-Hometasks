cmake_minimum_required(VERSION 3.8)
project(task6_hardcore)

set(CMAKE_C_STANDARD 99)

set(SOURCE_FILES main.c linkedList.c linkedList.h hashTable.c hashTable.h pair.c pair.h stringArray.h stringArray.c lineEnd.h fileIO.c fileIO.h)
add_executable(task6_hardcore ${SOURCE_FILES})
