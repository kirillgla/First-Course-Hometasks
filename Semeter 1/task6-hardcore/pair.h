#ifndef PAIR_H
#define PAIR_H

#include <math.h>
#include <limits.h>

#define HASHTABLE_KEY_TYPE char *
#define HASHTABLE_VALUE_TYPE unsigned int
#define HASHTABLE_VALUE_NOT_FOUND UINT_MAX

typedef struct
{
    HASHTABLE_KEY_TYPE key;
    HASHTABLE_VALUE_TYPE value;
} Pair;

Pair *buildPair(HASHTABLE_KEY_TYPE, HASHTABLE_VALUE_TYPE);

int isValue(HASHTABLE_VALUE_TYPE);

void printPair(Pair);

#endif /* PAIR_H */
