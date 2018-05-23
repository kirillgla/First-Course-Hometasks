#include <malloc.h>
#include <stdio.h>
#include <string.h>

#include "fileIO.h"

/// @private
/// @param buf buffer as part of string to be analyzed
/// @param length max number of characters to be processed
unsigned int getStringLengthDelta(const char *buf, unsigned int length)
{
    unsigned int result = 0;
    while (1)
    {
        if (result >= length || buf[result] == '\n')
            return result;
        result++;
    }
}

/// @private
void saveLine(HashTable *table, char **currentLine, unsigned int *currentLineLength)
{
    (*currentLine)[*currentLineLength] = '\0';
    if (incElementAt(table, *currentLine) == 0)
    {
        free(*currentLine);
    }
    *currentLine = NULL;
    *currentLineLength = 0;
}

int fillTable(HashTable *table, char *path)
{
    FILE *fileStreamIn = fopen(path, "r");
    if (fileStreamIn == NULL)
    {
        perror("fopen");
        return 1;
    }

    char *buf = (char *) malloc(sizeof(char) * BUFFER_SIZE);

    unsigned int result = 0;
    char *currentLine = NULL;
    unsigned int currentLineLength = 0;
    do
    {
        result = fread(buf, sizeof(char), BUFFER_SIZE, fileStreamIn);
        unsigned int offset = 0;
        do
        {
            unsigned int lengthDelta = getStringLengthDelta(buf + offset, result - offset);
            if (lengthDelta == 0)
            {
                if (currentLine !=  NULL)
                {
                    saveLine(table, &currentLine, &currentLineLength);
                }
                // Empty lines are ignored.
                // If necessary, they can  be handled right here.
            }
            else
            {
                currentLineLength += lengthDelta;

                if (currentLine == NULL)
                {
                    currentLine = (char *) malloc(sizeof(char) * (currentLineLength + 1));
                }
                else
                {
                    currentLine = (char *) realloc(currentLine, sizeof(char) * (currentLineLength + 1));
                }
                memcpy(currentLine + currentLineLength - lengthDelta, buf + offset, sizeof(char) * lengthDelta);
                if (offset + lengthDelta < result)
                {
                    saveLine(table, &currentLine, &currentLineLength);
                }
            }
            offset += lengthDelta + 1;
        }
        while (offset < result);
    }
    while (result == BUFFER_SIZE);

    // Last line might have not been saved,
    // in case it is not terminated with '\n'
    if (currentLine != NULL)
    {
        saveLine(table, &currentLine, &currentLineLength);
    }

    fclose(fileStreamIn);
    free(buf);
    return 0;
}

int saveText(HashTable *table, StringArray *array, const char *path)
{
    // FILE *fileStreamOut = fopen("C:\\Users\\kiril\\Desktop\\resources\\result.txt", "w");
    FILE *fileStreamOut = fopen(path, "w");
    // FILE *fileStreamOut = stdout;
    if (fileStreamOut == NULL)
    {
        perror("fopen");
        return 1;
    }
    for (unsigned int i = 0; i < array->length; i++)
    {
        unsigned int entries = getValue(table, array->data[i]);
        for (unsigned int j = 0; j < entries; j++)
        {
            fprintf(fileStreamOut, "%s\n", array->data[i]);
        }
    }
    fclose(fileStreamOut);
    return 0;
}
