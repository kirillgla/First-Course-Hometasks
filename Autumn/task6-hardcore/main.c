#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

#include "hashTable.h"
#include "fileIO.h"

int mstrcmp(const void *a, const void *b)
{
    return strcmp(*(char **) a, *(char **) b);
}

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "usage: %s <source file name>\n", argv[0]);
        return 1;
    }

    printf("Sorting text file.\n");

    clock_t start = clock();

    HashTable *table = buildHashTable();

    printf("Filling hashTable...\n");

    if (fillTable(table, argv[1]))
        return 1;

    printf("Retriving keys...\n");

    StringArray *array = getKeys(table);

    printf("Sorting keys...\n");

    qsort(array->data, array->length, sizeof(char *), &mstrcmp);

    printf("Saving results...\n");

    if (saveText(table, array, argv[1]))
        return 1;

    freeStringArray(array);
    freeHashTable(table);

    clock_t end = clock();
    printf("\nDone, %.02f sec\n", (double) (end - start) / CLOCKS_PER_SEC);

    return 0;
}
