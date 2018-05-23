#ifndef LINKEDLIST_H
#define LINKEDLIST_H

#include "pair.h"

#define LIST_VALUE_TYPE Pair

typedef struct Element
{
    LIST_VALUE_TYPE value;
    struct Element *next;
} Element;

typedef struct
{
    Element *first;
    int length;
} LinkedList;

int equal(LIST_VALUE_TYPE, LIST_VALUE_TYPE);

Element *buildElement(LIST_VALUE_TYPE);

LinkedList *buildLinkedList();

// Ok, well, this method is not that abstract. But still let's keep it here
void removeKeyFromList(LinkedList *list, HASHTABLE_KEY_TYPE key);

void addToList(LinkedList*, LIST_VALUE_TYPE);

// Removes first occurance
void removeFromList(LinkedList*, LIST_VALUE_TYPE);

void printList(LinkedList*);

void freeList(LinkedList*);

void freeListContents(LinkedList*);

#endif /* LINKEDLIST_H */
