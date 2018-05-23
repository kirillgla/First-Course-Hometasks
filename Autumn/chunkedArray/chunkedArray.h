#ifndef CHUNKEDARRAY_H
#define CHUNKEDARRAY_H

#define ARRAY_VALUE_TYPE int

#define VALUE_ERROR 0xBAADF00D

#define BASE_LENGTH 8

typedef struct
{
    ARRAY_VALUE_TYPE **content;
    int linesNumber; // Number of lines in chunkedArray
    int lastLineLength; // Number of elements in the last line
} ChunkedArray;

ChunkedArray *buildChunkedArray();

void printArray(ChunkedArray*);

void addToEnd(ChunkedArray*, ARRAY_VALUE_TYPE);

ARRAY_VALUE_TYPE at(ChunkedArray*, int);

void freeChunkedArray(ChunkedArray*);

#endif /* CHUNKEDARRAY_H */