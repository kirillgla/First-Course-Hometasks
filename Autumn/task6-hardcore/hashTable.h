#ifndef HASHTABLE_H
#define HASHTABLE_H

#include "linkedList.h"
#include "pair.h"
#include "stringArray.h"

// Initial number of buckets
#define BASE_BINS 8

// Max bucket length
#define MAX_LENGTH 32

typedef struct
{
    LinkedList *bins;
    int length;
} HashTable;

unsigned int hash(HASHTABLE_KEY_TYPE);

HashTable *buildHashTable();

unsigned int getElementsNumber(HashTable *);

StringArray *getKeys(HashTable *);

/// Adds element if necessary
/// @return 0 if element already existed
///         1 if it has been added
int incElementAt(HashTable *, HASHTABLE_KEY_TYPE);

void extendHashTable(HashTable *);

void addToHashTable(HashTable *, HASHTABLE_KEY_TYPE, HASHTABLE_VALUE_TYPE);

HASHTABLE_VALUE_TYPE getValue(HashTable *, HASHTABLE_KEY_TYPE);

void printHashTable(HashTable *);

void printHashTableRaw(HashTable *);

void freeHashTable(HashTable *);

#endif /* HASHTABLE_H */
