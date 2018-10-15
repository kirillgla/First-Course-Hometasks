#include <stdio.h>

#include "myMemoryManager.h"

/// Format is expected to contain
/// array data specifier and %s.
/// Keep in mind this is macro, not a method.
/// No ++x and stuff.
#define printArray(length, array, format)\
            printf("%s:\n[", #array);\
            for (int i = 0; i < (length); i++)\
                printf(format, (array)[i], i == (length) - 1? "]\n" : ", ")

// Not as safe as in C#
// Who cares safety in C after all?
#define nameof(variable) #variable

const size_t OBSERVED_ALLOCATION_MAP = 64;

#if OBSERVED_ALLOCATION_MAP % 8 != 0
#error OBSERVED_ALLOCATION_MAP should be a multiple of 8
#endif

int main()
{
    printf("Testing my malloc.\n");

    // Initialize

    if (init() != 0)
    {
        printf("Initialization error.\n");
        return 1;
    }
    printAllocationMap(OBSERVED_ALLOCATION_MAP);

    // Create first array

    int *array1 = myMalloc(sizeof(int) * 4);
    if (array1 == NULL)
    {
        printf("Error my-allocating %s.\n", nameof(array1));
        finish();
        return 1;
    }
    for (int i = 0; i < 4; i++)
    {
        array1[i] = i;
    }
    printf("Allocated and filled %s.\n", nameof(array1));
    printArray(4, array1, "%d%s");
    printAllocationMap(OBSERVED_ALLOCATION_MAP);

    // Create second array

    double *array2 = myMalloc(sizeof(double) * 2);
    if (array2 == NULL)
    {
        printf("Error my-allocating %s.\n", nameof(array2));
        finish();
        return 1;
    }
    for (int i = 0; i < 2; i++)
    {
        array2[i] = (double) i;
    }
    printf("Allocated and filled %s.\n", nameof(array2));
    printArray(2, array2, "%.2lf%s");
    printAllocationMap(OBSERVED_ALLOCATION_MAP);

    // Recreate first array

    array1 = myRealloc(array1, sizeof(int) * 5);
    if (array1 == NULL)
    {
        printf("Error my-reallocating %s.\n", nameof(array1));
        finish();
        return 1;
    }
    for (int i = 4; i < 5; i++)
    {
        array1[i] = 42;
    }
    printf("Reallocated and completed %s.\n", nameof(array1));
    printArray(5, array1, "%d%s");
    printAllocationMap(OBSERVED_ALLOCATION_MAP);

    // Free first array

    myFree(array1);
    printf("Freed %s.\n", nameof(array1));
    printAllocationMap(OBSERVED_ALLOCATION_MAP);

    // Reallocate second array

    array2 = myRealloc(array2, sizeof(double) * 3);
    if (array2 == NULL)
    {
        printf("Error my-reallocating %s.\n", nameof(array2));
        finish();
        return 1;
    }
    for (int i = 2; i < 3; i++)
    {
        array2[i] = (double) i;
    }
    printf("Reallocated and completed %s.\n", nameof(array2));
    printArray(3, array2, "%lf%s");
    printAllocationMap(OBSERVED_ALLOCATION_MAP);

    finish();
    return 0;
}
