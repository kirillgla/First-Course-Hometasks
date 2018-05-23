#ifndef MY_MEMORY_MANAGER_H
#define MY_MEMORY_MANAGER_H

// Can this be 32?
// Required so that allocationMap would always work properly.
// Don't change this!
#define SIZE_REQUIREMENT 64

// A megabyte is enough for most reasonable purposes
#define ALLOCATED_MEMORY_SIZE (1024 * 1024)

const unsigned int allocationMapSize;

#if ALLOCATED_MEMORY_SIZE % SIZE_REQUIREMENT != 0
#error ALLOCATED_MEMORY_SIZE should be a multiple of SIZE_REQUIREMENT
#endif



/// @return 0 on success, non-zero value otherwise
int init();

/// Prints allocation of first bytes in map
/// 0 stands for non-my-allocated byte,
//  (pun intended)
/// 1 stands for my-allocated one.
void printAllocationMap(size_t);

/// @return pointer to allocated memory on success,
/// NULL otherwise
void *myMalloc(size_t);

/// @return 0 on success, non-zero value otherwise
int myFree(void*);

/// @return pointer to allocated memory on success,
/// NULL otherwise
void *myRealloc(void*, size_t);

/// Always succeeds
void finish();

#endif /* MY_MEMORY_MANAGER_H */
