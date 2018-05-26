#include <stdio.h>
#include <malloc.h>

#include "pair.h"

Pair *buildPair(HASHTABLE_KEY_TYPE key, HASHTABLE_VALUE_TYPE value)
{
    Pair *result = malloc(sizeof(Pair));
    result->key = key;
    result->value = value;
    return result;
}

int isValue(HASHTABLE_VALUE_TYPE value)
{
    return value != HASHTABLE_VALUE_NOT_FOUND;
}

void printPair(Pair pair)
{
    printf("%s: %u", pair.key, pair.value);
}
