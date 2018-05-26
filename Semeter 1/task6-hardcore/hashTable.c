#include <stdio.h>
#include <malloc.h>
#include <string.h>

#include "hashTable.h"

unsigned int hash(char *str)
{
    unsigned long hash = 5381;
    int c;

    while ((c = *str++) != '\0')
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

HashTable *buildHashTable()
{
    HashTable *result = malloc(sizeof(HashTable));
    result->length = BASE_BINS;
    result->bins = malloc(sizeof(LinkedList) * BASE_BINS);
    for (int i = 0; i < BASE_BINS; i++)
        result->bins[i] = *buildLinkedList();
    return result;
}

unsigned int getElementsNumber(HashTable *table)
{
    unsigned int result = 0;
    for (int i = 0; i < table->length; i++)
        result += (table->bins + i)->length;
    return result;
}

StringArray *getKeys(HashTable *table)
{
    StringArray *array = (StringArray *) malloc(sizeof(StringArray));
    array->length = getElementsNumber(table);
    if (array->length == 0)
    {
        array->data = 0;
        return array;
    }

    array->data = (char **) malloc(sizeof(char *) * array->length);

    unsigned int index = 0;
    unsigned int bucketIndex = 0;
    Element *current = table->bins[0].first;
    while (1)
    {
        if (index == array->length)
            break;

        if (current == NULL)
        {
            bucketIndex++;
            current = table->bins[bucketIndex].first;
            continue;
        }

        array->data[index] = current->value.key;
        index++;

        current = current->next;
    }

    return array;
}

int incElementAt(HashTable *table, HASHTABLE_KEY_TYPE key)
{
    unsigned int bin = hash(key) % table->length;
    Element *current = table->bins[bin].first;
    int result = 0;
    while (1)
    {
        if (current == NULL)
        {
            pushValueToEnd(table->bins + bin, *buildPair(key, 0));
            current = table->bins[bin].last;
            result = 1;
        }
        if (strcmp(current->value.key, key) == 0)
        {
            current->value.value++;
            return result;
        }
        current = current->next;
    }
}

void extendHashTable(HashTable *table)
{
    fprintf(stderr, "Called extendHashTable!\n");
    int newLength = table->length + BASE_BINS;
    LinkedList *newBins = malloc(sizeof(LinkedList) * newLength);
    for (int i = 0; i < newLength; i++)
        newBins[i] = *buildLinkedList();
    for (int i = 0; i < table->length; i++)
    {
        Element *current = (table->bins + i)->first;
        while (current != NULL)
        {
            int index = hash(current->value.key) % newLength;
            pushValueToEnd(newBins + index, current->value);
            current = current->next;
        }
    }
    for (int i = 0; i < table->length; i++)
        freeListContents(table->bins + i);
    free(table->bins);
    table->bins = newBins;
    table->length = newLength;
}

void addToHashTable(HashTable *table, HASHTABLE_KEY_TYPE key, HASHTABLE_VALUE_TYPE value)
{
    if (!isValue(value))
        return;
    int index = hash(key) % table->length;
    pushValueToEnd(table->bins + index, *buildPair(key, value));
    if (table->bins[index].length >= MAX_LENGTH)
    {
        extendHashTable(table);
    }
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
        printList(table->bins + i, &printPair);
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
