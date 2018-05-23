#ifndef LINKEDLIST_H
#define LINKEDLIST_H

/* ==== ==== implementation-dependent ==== ==== */

#ifndef LIST_VALUE_TYPE
#define LIST_VALUE_TYPE unsigned int
#endif

int equal(LIST_VALUE_TYPE, LIST_VALUE_TYPE);

void printListElement(LIST_VALUE_TYPE, char *);

/* ==== ==== implementation-independent ==== ==== */

typedef struct Element
{
    LIST_VALUE_TYPE value;
    struct Element *next;
    struct Element *previous;
} Element;

typedef struct
{
    Element *first;
    unsigned int length;
    Element *last;
} LinkedList;

Element *buildElement(LIST_VALUE_TYPE);

LinkedList *buildLinkedList();

int listIsValid(LinkedList *list);

void pushElementToEnd(LinkedList *, Element *);

void pushValueToEnd(LinkedList *, LIST_VALUE_TYPE);

void pushElementToStart(LinkedList *, Element *);

void pushValueToStart(LinkedList *, Element*);

void pushAllValuesToEnd(LinkedList *, int, ...);

void pushListToEnd(LinkedList*, LinkedList *);

void pushListToStart(LinkedList *, LinkedList *);

LinkedList *splitListEnd(LinkedList *, unsigned int);

LinkedList *splitListStart(LinkedList *, unsigned int);

/// Removes first occurance
void removeValueFromList(LinkedList *, LIST_VALUE_TYPE);

void removeElementFromList(LinkedList *, Element *);

Element *popFirst(LinkedList *);

Element *popLast(LinkedList *);

void printList(LinkedList *, char *);

/// Frees the pointer
void freeList(LinkedList *);

/// Leaves the pointer
void freeListContents(LinkedList *);

#endif /* LINKEDLIST_H */
