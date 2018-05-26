#ifndef HASHTABLE_H
#define HASHTABLE_H

#include "linkedList.h"
#include "pair.h"

// Initial number of buckets
#define BASE_LENGTH 4

// Max bucket length
#define MAX_LENGTH 4

typedef struct
{
    LinkedList *bins;
    int length;
} HashTable;

int hash(HASHTABLE_KEY_TYPE);

HashTable *buildHashTable();

int getElementsNumber(HashTable*);

void extendHashTable(HashTable*);

void addToHashTable(HashTable*, HASHTABLE_KEY_TYPE, HASHTABLE_VALUE_TYPE);

void removeFromHashTable(HashTable*, HASHTABLE_KEY_TYPE);

HASHTABLE_VALUE_TYPE getValue(HashTable*, HASHTABLE_KEY_TYPE);

void printHashTable(HashTable*);

void printHashTableRaw(HashTable*);

void freeHashTable(HashTable*);

#endif /* HASHTABLE_H */
