#ifndef TASK6_MSTRING_H
#define TASK6_MSTRING_H

#include <stdlib.h>

#include "mappedFile.h"

/// @return mapped string length
///     including finishing CRLF sequence
size_t mstrlen(const char *);

char *mstrcpy(char *, const char *, unsigned int);

unsigned int mLinesCount(MappedFile *);

int mstrcmp(const void *, const void *);

int sort(MappedFile *, char *);

#endif //TASK6_MSTRING_H
