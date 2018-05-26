#include <stdio.h>
#include <malloc.h>
#include <stdarg.h>

#include "linkedList.h"

/* ==== ==== implementation-dependent ==== ==== */

int equal(LIST_VALUE_TYPE first, LIST_VALUE_TYPE second)
{
    return first == second;
}

void printListElement(LIST_VALUE_TYPE value, char *format)
{
    printf(format, value);
}

/* ==== ==== implementation-independent ==== ==== */

Element *buildElement(LIST_VALUE_TYPE value)
{
    Element *result = malloc(sizeof(Element));
    result->value = value;
    result->next = NULL;
    result->previous = NULL;
    return result;
}

LinkedList *buildLinkedList()
{
    LinkedList *result = malloc(sizeof(LinkedList));
    result->first = NULL;
    result->last = NULL;
    result->length = 0;
    return result;
}

int listIsValid(LinkedList *list)
{
    // Check that values stored in list
    // do not contradict each other.
    // It is unclear whether it is possible
    // to simplify this condition or not.
    return list != NULL
           && (list->first == NULL && list->last == NULL && list->length == 0
               || list->first != NULL && list->last != NULL && list->length != 0
           );
}

// private
void unsafeAddElementToList(LinkedList *list, Element *element)
{
    if (list->length == 0)
    {
        list->first = element;
        list->last = element;
        list->length = 1;
        return;
    }
    list->last->next = element;
    element->previous = list->last;
    list->length++;
    list->last = element;
}

void pushElementToEnd(LinkedList *list, Element *element)
{
    if (!listIsValid(list))
    {
        printf("Error: linked list is likely corrupted.");
        return;
    }
    unsafeAddElementToList(list, element);
}

void pushValueToEnd(LinkedList *list, LIST_VALUE_TYPE value)
{
    if (!listIsValid(list))
    {
        printf("Error: linked list is likely corrupted.");
        return;
    }
    Element *newElement = buildElement(value);
    unsafeAddElementToList(list, newElement);
}

void pushElementToStart(LinkedList *list, Element *element)
{
    printf("pushElementToStart is not yet implemented.");
}

void pushValueToStart(LinkedList *list, Element *element)
{
    printf("pushValueToStart is not yet implemented.");
}

void pushAllValuesToEnd(LinkedList *list, int count, ...)
{
    va_list arg_list;
    va_start(arg_list, count);
    for (int i = 0; i < count; i++)
    {
        LIST_VALUE_TYPE arg = va_arg(arg_list, LIST_VALUE_TYPE);
        pushValueToEnd(list, arg);
    }
}

void pushListToEnd(LinkedList *list, LinkedList *addedList)
{
    // TODO
}

void pushListToStart(LinkedList *list, LinkedList *addedList)
{
    if (list == NULL || addedList == NULL || addedList->length == 0)
        return;

    if (list->length == 0)
    {
        list->last = addedList->last;
    }
    else
    {
        addedList->last->next = list->first;
        list->first->previous = addedList->last;
    }

    list->first = addedList->first;
    list->length += addedList->length;

    addedList->first = NULL;
    addedList->last = NULL;
    addedList->length = 0;
}

LinkedList *splitListEnd(LinkedList *list, unsigned int ammount)
{
    if (!listIsValid(list) || ammount == 0)
        return NULL;

    LinkedList *result = buildLinkedList();

    if (list->length <= ammount)
    {
        result->first = list->first;
        result->last = list->last;
        result->length = list->length;

        list->first = NULL;
        list->last = NULL;
        list->length = 0;
        return result;
    }

    Element *element = list->last;
    int elementsSelected = 1;

    while (elementsSelected < ammount)
    {
        element = element->previous;
        elementsSelected++;
    }

    result->last = list->last;
    result->first = element;
    result->length = ammount;

    list->last = element->previous;
    list->length -= ammount;

    element->previous->next = NULL;
    element->previous = NULL;

    return result;
}

LinkedList *splitListStart(LinkedList *list, unsigned int ammount)
{
    if (!listIsValid(list) || ammount == 0)
        return NULL;

    LinkedList *result = buildLinkedList();

    if (list->length <= ammount)
    {
        result->first = list->first;
        result->last = list->last;
        result->length = list->length;

        list->first = NULL;
        list->last = NULL;
        list->length = 0;
        return result;
    }

    Element *element = list->first;
    int elementsSelected = 1;

    while (elementsSelected < ammount)
    {
        element = element->next;
        elementsSelected++;
    }

    result->first = list->first;
    result->last = element;
    result->length = ammount;

    list->first = element->next;
    list->length -= ammount;

    element->next->previous = NULL;
    element->next = NULL;

    return result;
}

void removeValueFromList(LinkedList *list, LIST_VALUE_TYPE value)
{
    if (list == NULL || list->first == NULL)
        return;
    if (equal(list->first->value, value))
    {
        Element *tmp = list->first;
        list->first = list->first->next;
        if (list->first != NULL)
            list->first->previous = NULL;
        list->length--;
        if (list->last == tmp)
            list->last = NULL;
        free(tmp);
        return;
    }
    Element *current = list->first;
    while (current->next != NULL)
    {
        if (equal(current->next->value, value))
        {
            removeElementFromList(list, current);
            return;
        }
        current = current->next;
    }
}

void removeElementFromList(LinkedList *list, Element *element)
{
    if (element == NULL)
        return;

    if (element->next != NULL)
        element->next->previous = element->previous;

    if (element->previous != NULL)
        element->previous->next = element->next;

    list->length--;

    if (list->first == element)
        list->first = element->next;

    if (list->last == element)
        list->last = element->previous;

    free(element);
}

Element *popFirst(LinkedList *list)
{
    if (list == NULL || list->length == 0)
        return NULL;

    Element *first = list->first;
    list->length--;

    if (list->length == 0)
    {
        list->first = NULL;
        list->last = NULL;
        return first;
    }

    first->next->previous = NULL;
    list->first = first->next;
    first->next = NULL;
    return first;
}

Element *popLast(LinkedList *list)
{
    if (list == NULL || list->length == 0)
        return NULL;

    Element *last = list->last;
    list->length--;

    if (list->length == 0)
    {
        list->last = NULL;
        list->first = NULL;
        return last;
    }

    last->previous->next = NULL;
    list->last = last->previous;
    last->previous = NULL;
    return last;
}

void printList(LinkedList *list, char *format)
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
        printListElement(current->value, format);
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
