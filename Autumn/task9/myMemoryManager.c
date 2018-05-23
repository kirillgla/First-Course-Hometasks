#include <stdio.h>
#include <malloc.h>

#include "myMemoryManager.h"

unsigned char *allocatedMemory;

// Assuming "direct bit order"
// is the one on big-endian platforms.
//
// If we reversed direct bit order inside int
// i-th bit of this map would mean
// whether i-th byte of allocated memory
// is my-allocated or not.
//
// That "if" makes things difficult to understand
// but optimizes and simplifies the code.
//
// My-allocated objects end with
// exactly one byte marked as non-my-allocated
// (i. e. zero bit in this map).
// Therefore, minimum allocation size is 2 bytes.
//
// For example, 12-bytes object ("the object") would look like:
// 0000000000 111111111110 0000000000
// ^whatever^ ^the object^ ^whatever^
//
// If we had another 4-bytes object ("o2") after it:
// 0000000000 111111111110 1110 0000000000
// ^whatever^ ^the object^ ^o2^ ^whatever^
unsigned int *allocationMap;

const unsigned int allocationMapSize = ALLOCATED_MEMORY_SIZE / (sizeof(unsigned int) * 8);

int init()
{
    allocationMap = calloc(allocationMapSize, sizeof(unsigned int));
    // allocationMap = malloc(sizeof(unsigned int) * allocationMapSize);
    // markRange(0, allocationMapSize, 0);
    if (allocationMap == NULL)
        return 1;

    allocatedMemory = malloc(sizeof(unsigned char) * ALLOCATED_MEMORY_SIZE);
    if (allocatedMemory == NULL)
    {
        free(allocationMap);
        return 1;
    }

    return 0;
}

/// Returns whether selected byte
/// is my-allocated or not,
/// according to allocationMap.
/// All bytes out of my-allocation range
/// are considered my-allocated.
int isAllocated(int index)
{
    if (index >= allocationMapSize)
    {
        printf("Warning: checking my-allocation in non-allocated index.\n");
        return 1;
    }

    // Note: here comes the simplification
    // mentioned before allocationMap.
    return allocationMap[index / (sizeof(unsigned int) * 8)] &
            (1 << (index % (sizeof(unsigned int) * 8)));
}

/// @param marker is considered a boolean value
/// @return 0 on success, non-zero value otherwise
int setAllocated(int index, int marker)
{
    if (index >= allocationMapSize)
    {
        printf("Error: attempting to change my-allocation in non-allocated index.\n");
        return 1;
    }

    if (marker)
    {
        allocationMap[index / (sizeof(unsigned int) * 8)] |=
                1 << (index % (sizeof(unsigned int) * 8));
    }
    else
    {
        allocationMap[index / (sizeof(unsigned int) * 8)] &=
                ~(1 << (index % (sizeof(unsigned int) * 8)));
    }
    return 0;
}

void printAllocationMap(size_t length)
{
    printf("Allocation map:\n");
    for (int i = 0; i < length; i++)
    {
        printf(isAllocated(i)? "1": "0");
        if (i % 8 == 7)
        {
            printf(" ");
        }
    }
    printf("\n\n");
}

/// Marks range as my-allocated/non-my-allocated
/// @param marker is considered a boolean value
/// @return 0 on success, non-zero value otherwise
int setAllocatedRange(int start, int length, int marker)
{
    for (int i = 0; i < length; i++)
    {
        if (setAllocated(start + i, marker))
            return 1;
    }

    return 0;
}

/// @return next free space in allocatedMemory
/// On failure, returns -1.
int nextIndex(int index)
{
    // Skip non-my-allocated region

    while (isAllocated(index) == 0)
    {
        index++;
        if (index >= allocationMapSize)
            return -1;
    }

    // Skip my-allocated region

    if (index + 2 >= allocationMapSize)
        return -1;

    // Free space requires at least 3
    // non-my-allocated bytes so that
    // objects can be separated properly
    // (first byte is ending of already existing object,
    // second one is the beginning of new region,
    // third one is reserved for it's ending).
    while (isAllocated(index) || isAllocated(index + 1) || isAllocated(index + 2))
    {
        index++;
        if (index + 2 >= allocationMapSize)
            return -1;
    }

    return index + 1;
}

int enoughSpace(int index, size_t size)
{
    // This safety check might be redundant
    if (index >= allocationMapSize)
        return 0;

    int maxIndex = index;
    while (!isAllocated(maxIndex))
    {
        if (maxIndex - index + 1>= size)
            return 1;

        if (maxIndex + 1 >= allocationMapSize)
            return 0;

        maxIndex++;
    }
    return 0;
}

/// @return index of place in allocatedMemory
/// followed by enough space to contain @param size bytes.
/// On failure, returns -1.
int findIndex(size_t size)
{
    int index = 0;
    while (index != -1)
    {
        if (enoughSpace(index, size))
            return index;

        index = nextIndex(index);
    }
    return -1;
}

void *myMalloc(size_t size)
{
    // Since minimum my-allocatable object size is 2 bytes,
    // and negative sizes make no sense:
    if (size < 1)
    {
        return NULL;
    }
    else if (size == 1)
    {
        size = 2;
    }

    int index = findIndex(size);
    if (index == -1)
        return NULL;

    setAllocatedRange(index, size - 1, 1);
    return allocatedMemory + index;
}

/// assuming @param start is actually start
size_t objectSize(int start)
{
    size_t length = 0;
    while (isAllocated(start + length))
    {
        length++;
    }
    // Since first non-my-allocated byte
    // is actually part of this object,
    return length + 1;
}

int myFree(void *pointer)
{
    int startIndex = (int) ((unsigned char*) pointer - allocatedMemory);
    if (startIndex < 0 || startIndex >= allocationMapSize)
    {
        printf("Error: pointer is not in allocation range.\n");
        return 1;
    }

    // No need to mark the last byte as non-my-allocated
    // since it is already marked like that
    setAllocatedRange(startIndex, objectSize(startIndex) - 1, 0);

    return 0;
}

void *myRealloc(void *pointer, size_t newSize)
{
    if (newSize <= 1)
    {
        newSize = 2;
    }

    int initialStartIndex = (int) ((unsigned char*) pointer - allocatedMemory);
    if (initialStartIndex < 0 || initialStartIndex >= allocationMapSize)
    {
        printf("Error: current object is not my-allocated.\n");
        return NULL;
    }

    int initialLength = objectSize(initialStartIndex);

    if (initialLength > newSize)
    {
        // Following operation also marks last byte in reallocated region
        // as non-my-allocated.
        setAllocatedRange(initialStartIndex + newSize - 1, initialLength - newSize, 0);
        return pointer;
    }

    if (enoughSpace(initialStartIndex + initialLength, newSize - initialLength))
    {
        // Following operation also marks last byte in reallocated region
        // as non-my-allocated.
        setAllocatedRange(initialStartIndex + initialLength - 1, newSize - initialLength, 1);
        return pointer;
    }

    unsigned char *newPointer = myMalloc(newSize);

    if (newPointer == NULL)
        return NULL;

    for (int i = 0; i < newSize; i++)
    {
        newPointer[i] = ((unsigned char*)pointer)[i];
    }

    myFree(pointer);

    return newPointer;
}

void finish()
{
    free(allocatedMemory);
    free(allocationMap);
    allocatedMemory = NULL;
    allocationMap = NULL;
}
