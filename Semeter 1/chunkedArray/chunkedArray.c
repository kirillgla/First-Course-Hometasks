#include <stdio.h>
#include <malloc.h>

#include "chunkedArray.h"

ChunkedArray *buildChunkedArray()
{
    ChunkedArray *result = malloc(sizeof(ChunkedArray));
    result->linesNumber= 1;
    result->lastLineLength = 0;
    result->content = malloc(sizeof(ARRAY_VALUE_TYPE*));
    result->content[0] = malloc(sizeof(ARRAY_VALUE_TYPE) * BASE_LENGTH);
    return result;
}

void printArray(ChunkedArray *array)
{
    printf("{\n");
    for (int i = 0; i < array->linesNumber; i++)
    {
        printf("  [");
        if (i != array->linesNumber - 1)
        {
            for (int j = 0; j < BASE_LENGTH; j++)
            {
                printf(j == BASE_LENGTH - 1? "%d" : "%d, ", array->content[i][j]);
            }
        }
        else
        {
            for (int j = 0; j < array->lastLineLength; j++)
            {
                printf(j == array->lastLineLength - 1? "%d" : "%d, ", array->content[i][j]);
            }
        }
        printf("]\n");
    }
    printf("}\n");
}

void addToEnd(ChunkedArray *array, ARRAY_VALUE_TYPE value)
{
    if (value == VALUE_ERROR)
        return; // We don't want such confusing values to be present in our array

    array->content[array->linesNumber - 1][array->lastLineLength] = value;
    array->lastLineLength++;

    if (array->lastLineLength == BASE_LENGTH)
    {
        array->linesNumber++;
        array->lastLineLength = 0;
        // Following line copies references to all existing lines
        array->content = realloc(array->content, sizeof(ARRAY_VALUE_TYPE) * array->linesNumber);
        array->content[array->linesNumber - 1] = malloc(sizeof(ARRAY_VALUE_TYPE) * BASE_LENGTH);
    }
}

ARRAY_VALUE_TYPE at(ChunkedArray *array, int index)
{
    int line = index / BASE_LENGTH;
    if (line >= array->linesNumber)
        return VALUE_ERROR;
    int indexInLine = index % BASE_LENGTH;
    if (line == array->linesNumber - 1 && indexInLine >= array->lastLineLength)
        return VALUE_ERROR;
    return array->content[line][indexInLine];
}

void freeChunkedArray(ChunkedArray *array)
{
    if (array == NULL)
        return;
    if (array->content != NULL)
    {
        for (int i = 0; i < array->linesNumber; i++)
            free(array->content[i]);
        free(array->content);
    }
    free(array);
}
