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
    return !isnan(value);
}

void printPair(Pair pair)
{
    printf("%d: %.2lf", pair.key, pair.value);
}
