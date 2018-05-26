#include <stdio.h>
#include <time.h>
#include <fcntl.h>
#include <string.h>

#include "mstring.h"

// This line matters!
#include "lineEnd.h"

size_t mstrlen(const char *str)
{
    char *tmp = (char*) str;
#ifdef CRLF
    while (*tmp != '\r' || *(tmp + 1) != '\n')
        tmp++;
    return (size_t) (tmp - str + 2);
#else
    while (*tmp != '\n')
        tmp++;
    return (size_t) (tmp - str + 1);
#endif // CRLF

}

/// @private
/// @return number of meaningfully observable lines
/// i.e. lines that can be safely checked for being EOL
/// without leaving the file.
/// On windows, last byte in file can't be checked
/// since that would also require accessing
/// data outside of file
unsigned int getFileRange(MappedFile *file)
{
#ifdef CRLF
    return file->length - 1;
#else
    return file->length;
#endif
}

/// @private
int isEOL(const char *string)
{
#ifdef CRLF
    return *string == '\r' && *(string + 1) == '\n';
#else
    return *string == '\n';
#endif
}

/// @private
void fillPointerArray(MappedFile *file, char **pointers, unsigned int lines)
{
    if (pointers == NULL || file == NULL || file->data == NULL || lines == 0)
        return;

    pointers[0] = file->data;
    int arrayIndex = 1;
    char *text = file->data;
    do
    {
        text++;
        if (isEOL(text))
        {
            text += EOL_LENGTH;
            pointers[arrayIndex] = text;
            arrayIndex++;
        }
    }
    while (arrayIndex < lines);
}

/// @private
/// Changes given path to path to tmp file
/// Tmp file name is not exceptionally good
/// or exceptionally safe,
/// but it does the job
/// @return character changed
char makeTmpPath(char *path)
{
    unsigned int slashIndex = strlen(path);
    while (1)
    {
        if (path[slashIndex] == '\\' || path[slashIndex] == '/' || slashIndex == 0)
            break;
        slashIndex--;
    }
    if (path[slashIndex + 1] == 'A')
        path[slashIndex + 1] = 'B';
    else
        path[slashIndex + 1] = 'A';
}

unsigned int mLinesCount(MappedFile *file)
{
    unsigned int result = 0;
    char *string = file->data;
    unsigned int length = getFileRange(file);
    for (unsigned int i = 0; i < length; i++)
    {
        if (isEOL(string))
            result++;
        string++;
    }
    return result;
}

int mstrcmp(const void *first, const void *second)
{
    char *firstStr = *(char **) first;
    char *secondStr = *(char **) second;
    while (1)
    {
        if (isEOL(firstStr))
        {
            if (isEOL(secondStr))
                return 0;
            return -1;
        }
        // !isEOL(firstStr)
        if (isEOL(secondStr))
            return 1;
        // !isEOL(secondStr)
        if (*firstStr == *secondStr)
        {
            firstStr++;
            secondStr++;
            continue;
        }
        return (int) (*firstStr - *secondStr);
    }
}

char *mstrcpy(char *dst, const char *src, unsigned int len)
{
    char *initial_dst = dst;
    if (len < EOL_LENGTH)
        len = mstrlen(src);
    int len_4 = len / 4;
    int len_not_4 = len % 4;

    for (int i = 0; i < len_not_4; i++)
        *(dst++) = *(src++);

    int *int_dst = (int *) dst;
    int *int_src = (int *) src;

    for (int i = 0; i < len_4; i++)
    {
        *(int_dst++) = *(int_src++);
    }
    return initial_dst;
}

int sort(MappedFile *file, char *path)
{
    clock_t globalStart;
    clock_t globalEnd;
    clock_t start;
    clock_t end;
    printf("Sorting lines in text file...\n");
    globalStart = clock();

    printf("Counting lines... ");
    start = clock();
    unsigned int linesCount = mLinesCount(file);
    end = clock();
    printf("%u (%.02f seconds).\n", linesCount, (double) (end - start) / CLOCKS_PER_SEC);

    char **pointers = malloc(sizeof(char *) * linesCount);
    if (pointers == NULL)
    {
        perror("malloc");
        return 1;
    }

    printf("Filling pointer array... ");
    start = clock();
    fillPointerArray(file, pointers, linesCount);
    end = clock();
    printf("Done (%.02f seconds).\n", (double) (end - start) / CLOCKS_PER_SEC);

    printf("Sorting pointer array... ");
    start = clock();
    qsort(pointers, linesCount, sizeof(const char *), mstrcmp);
    end = clock();
    printf("Done (%.02f seconds).\n", (double) (end - start) / CLOCKS_PER_SEC);

    printf("Savig results... ");
    start = clock();
    makeTmpPath(path);
    remove(path);
    MappedFile *tmp = map(path, O_RDWR | O_CREAT | O_TRUNC, file->length);
    if (tmp == NULL)
        return 1;

    for (unsigned int i = 0, index = 0; i < linesCount; i++)
    {
        unsigned int length = mstrlen(pointers[i]);
        mstrcpy(tmp->data + index, pointers[i], length);
        index += length;
    }

    memcpy(file->data, tmp->data, file->length);
    end = clock();
    printf("Done (%.02f seconds).\n", (double) (end - start) / CLOCKS_PER_SEC);

    if (unmap(tmp))
        return 1;

    remove(path);

    free(pointers);

    globalEnd = clock();
    printf("Done. Total time: %.02f seconds.\n", (double) (globalEnd - globalStart) / CLOCKS_PER_SEC);
    return 0;
}
