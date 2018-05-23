#ifndef TASK6_HARDCORE_FILEIO_H
#define TASK6_HARDCORE_FILEIO_H

#include "hashTable.h"

#define BUFFER_SIZE 8192

int fillTable(HashTable *, char *);

int saveText(HashTable *, StringArray *, const char *);

#endif /* TASK6_HARDCORE_FILEIO_H */
