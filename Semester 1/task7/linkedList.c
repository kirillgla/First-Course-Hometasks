#include <stdio.h>
#include <malloc.h>

#include "linkedList.h"

int equal(LIST_VALUE_TYPE first, LIST_VALUE_TYPE second)
{
    return first.key == second.key;
}

Element *buildElement(LIST_VALUE_TYPE value)
{
    Element *result = malloc(sizeof(Element));
    result->value = value;
    result->next = NULL;
    return result;
}

LinkedList *buildLinkedList()
{
    LinkedList *result = malloc(sizeof(LinkedList));
    result->first = NULL;
    result->length = 0;
    return result;
}

void addToList(LinkedList *list, LIST_VALUE_TYPE value)
{
    if (list == NULL)
        return;
    Element *newElement = buildElement(value);
    if (list->first == NULL)
    {
        list->first = newElement;
        list->length = 1;
        return;
    }
    Element *current = list->first;
    while (current->next != NULL)
        current = current->next;
    current->next = newElement;
    list->length++;
}

void removeFromList(LinkedList *list, LIST_VALUE_TYPE value)
{
    if (list == NULL || list->first == NULL)
        return;
    if (equal(list->first->value, value))
    {
        Element *tmp = list->first;
        list->first = list->first->next;
        list->length--;
        free(tmp);
        return;
    }
    Element *current = list->first;
    while (current->next != NULL)
    {
        if (equal(current->next->value, value))
        {
            Element *tmp = current->next;
            current->next = current->next->next;
            list->length--;
            free(tmp);
            return;
        }
        current = current->next;
    }
}

void removeKeyFromList(LinkedList *list, HASHTABLE_KEY_TYPE key)
{
    Pair pair;
    pair.key = key;
    // pair.value doesn't matter
    removeFromList(list, pair);
}

void printList(LinkedList *list)
{
    if (list == NULL)
    {
        printf("NULL");
        return;
    }
    printf("[");
    Element *current = list->first;
    while (current != NULL)
    {
        printPair(current->value);
        if (current->next != NULL)
            printf(", ");
        current = current->next;
    }
    printf("]");
}

void freeList(LinkedList *list)
{
    if (list == NULL)
        return;
    freeListContents(list);
    free(list);
}

void freeListContents(LinkedList *list)
{
    if (list == NULL)
        return;
    Element *current = list->first;
    while (current != NULL)
    {
        Element *tmp = current;
        current = current->next;
        free(tmp);
    }
}
