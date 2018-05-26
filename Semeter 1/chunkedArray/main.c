#include <stdio.h>

#include "chunkedArray.h"

int main()
{
    printf("\nTesting chunked array.\n");
    printf("Base length: %d\n", BASE_LENGTH);
    ChunkedArray *array = buildChunkedArray();
    printf("0th element before filling the array: 0x%08X\n", at(array, 0));
    for (int i = 0; i < 10; i++)
    {
        addToEnd(array, i);
    }
    printf("Array after first filling:\n");
    printArray(array);
    // 10 is intentionally skipped
    for (int i = 11; i < 18; i++)
    {
        addToEnd(array, i);
    }

    printf("Array after second filling:\n");
    printArray(array);

    printf("Getting all elements:\n");
    int count = BASE_LENGTH * (array->linesNumber - 1) + array->lastLineLength;
    printf("[");
    for (int i = 0; i < count; i++)
    {
        printf(i == count - 1? "%d]\n" : "%d, ", at(array, i));
    }
    printf("Index out of range: 0x%08X\n", at(array, count));

    freeChunkedArray(array);
    printf("Done.\n");
    return 0;
}
