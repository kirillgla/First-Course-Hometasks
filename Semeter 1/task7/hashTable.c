#include <stdio.h>
#include <malloc.h>

#include "hashTable.h"

int hash(HASHTABLE_KEY_TYPE key)
{
    // Whatever
    return *(int*)&key;
}

HashTable *buildHashTable()
{
    HashTable *result = malloc(sizeof(HashTable));
    result->length = BASE_LENGTH;
    result->bins = malloc(sizeof(LinkedList) * BASE_LENGTH);
    for (int i = 0; i < BASE_LENGTH; i++)
        result->bins[i] = *buildLinkedList();
    return result;
}

int getElementsNumber(HashTable *table)
{
    int result = 0;
    for (int i = 0; i < table->length; i++)
        result += (table->bins + i)->length;
    return result;
}

void extendHashTable(HashTable *table)
{
    int newLength = table->length + BASE_LENGTH;
    LinkedList *newBins = malloc(sizeof(LinkedList) * newLength);
    for (int i = 0; i < newLength; i++)
        newBins[i] = *buildLinkedList();
    for (int i = 0; i < table->length; i++)
    {
        Element *current = (table->bins + i)->first;
        while (current != NULL)
        {
            int index = hash(current->value.key) % newLength;
            addToList(newBins + index, current->value);
            current = current->next;
        }
    }
    for (int i = 0; i < table->length; i++)
        freeListContents(table->bins + i);
    free(table->bins);
    table->bins = newBins;
    table->length = newLength;
}

void addToHashTable(HashTable* table, HASHTABLE_KEY_TYPE key, HASHTABLE_VALUE_TYPE value)
{
    if (!isValue(value))
        return;
    int index = hash(key) % table->length;
    addToList(table->bins + index, *buildPair(key, value));
    if (table->bins[index].length >= MAX_LENGTH)
    {
        extendHashTable(table);
    }
}

void removeFromHashTable(HashTable* table, HASHTABLE_KEY_TYPE key)
{
    int index = hash(key) % table->length;
    removeKeyFromList(table->bins + index, key);
}

HASHTABLE_VALUE_TYPE getValue(HashTable *table, HASHTABLE_KEY_TYPE key)
{
    int index = hash(key) % table->length;
    // Start viewing LinkedList
    Element *current = table->bins[index].first;
    while (current != NULL)
    {
        // In fact, we might need to implement equalsKeys() method
        if (current->value.key == key)
            return current->value.value;
        current = current->next;
    }
    return HASHTABLE_VALUE_NOT_FOUND;
}

void printHashTable(HashTable *table)
{
    if (getElementsNumber(table) == 0)
    {
        printf("[]\n");
        return;
    }
    printf("[\n");
    for (int i = 0; i < table->length; i++)
    {
        Element *element = (table->bins + i)->first;
        while (element != NULL)
        {
            printPair(element->value);
            printf("\n");
            element = element->next;
        }
    }
    printf("]\n");
}

void printHashTableRaw(HashTable *table)
{
    if (table->length > 99)
        return;
    // With 100+ entries this format of output is hardly useful anyway
    printf("[\n");
    for (int i = 0; i < table->length; i++)
    {
        printf("%02d: ", i);
        printList(table->bins + i);
        printf("\n");
    }
    printf("]\n");
}

void freeHashTable(HashTable *table)
{
    if (table == NULL)
        return;
    for (int i = 0; i < table->length; i++)
        freeListContents(table->bins + i);
    free(table->bins);
    free(table);
}
