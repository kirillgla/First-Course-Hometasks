#include <stdio.h>

#include "hashTable.h"

int main()
{
    printf("\nTesting hashTable.\n");
    printf("Base table length: %d\nMax bin length: %d\n\n", BASE_LENGTH, MAX_LENGTH);
    HashTable *table = buildHashTable();

    for (int i = 0; i < 20; i++)
        addToHashTable(table, i, (float) i);

    addToHashTable(table, 42, NAN);

    printf("After filling the table:\n");
    printHashTableRaw(table);
    
    for (int i = 7; i < 13; i++)
        removeFromHashTable(table, i);
    for (int i = 1024; i < 1034; i++)
        removeFromHashTable(table, i);

    printf("\nAfter removing elements:\n");
    printHashTableRaw(table);

    printf("\nA few selected values:\n");
    printf("5: %f\n", getValue(table, 5));
    printf("6: %f\n", getValue(table, 6));
    printf("6, once again: %f\n", getValue(table, 6));
    printf("7: %f\n", getValue(table, 7));
    printf("42: %f\n", getValue(table, 42));
    printf("1024: %f\n", getValue(table, 1024));

    freeHashTable(table);
    printf("\nDone.\n");
    return 0;
}
