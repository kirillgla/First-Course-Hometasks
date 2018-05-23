#include <fcntl.h>
#include <stdio.h>
#include <malloc.h>
#include <unistd.h>
#include <sys/stat.h>

// Alas, sorting file with no mmap usage
// turned out to be far more wired than I expected
#include <mman.h>

#include "mappedFile.h"

// This line matters!
#include "lineEnd.h"

/// @private
int endsWithEOL(const char *data, int length)
{
#ifdef CRLF
    return data[length - 2] == '\r' && data[length - 1] == '\n';
#else
    return data[length - 1] == '\n';
#endif // CRLF
}

/// @private
void appendEOL(int fileDes, char *map, struct stat *statistics)
{
    statistics->st_size++;
#ifdef CRLF
    statistics->st_size++;
#endif // CRLF
    ftruncate(fileDes, statistics->st_size);
    map[statistics->st_size - 1] = '\n';
#ifdef CRLF
    map[statistics->st_size - 2] = '\r';
#endif // CRLF
}

/// @return pointer to mmapped region on success,
/// NULL otherwise
/// Caller is responsible for munmapping MappedFile->data
/// and freeing MappedFile
MappedFile *map(const char *path, int rights, unsigned int minLength)
{
    int filedes = open(path, rights);
    if (filedes == -1)
    {
        perror("open");
        return NULL;
    }

    struct stat statistics;
    if (fstat(filedes, &statistics) == -1)
    {
        perror("fstat");
        return NULL;
    }

    if (!S_ISREG(statistics.st_mode))
    {
        fprintf(stderr, "%s is not a file\n", path);
        close(filedes);
        return NULL;
    }

    if (statistics.st_size == 0)
        statistics.st_size = (off_t) minLength;

    char *mappedFile = mmap(NULL, (size_t) statistics.st_size, PROT_READ | PROT_WRITE, MAP_SHARED, filedes, 0);
    if (mappedFile == MAP_FAILED)
    {
        perror("mmap");
        return NULL;
    }

    if (!endsWithEOL(mappedFile, statistics.st_size))
        appendEOL(filedes, mappedFile, &statistics);

    if (minLength > statistics.st_size)
    {
        ftruncate(filedes, (off_t) minLength);
        statistics.st_size = (off_t) minLength;
    }

    if (close(filedes) == -1)
    {
        perror("close");
        return NULL;
    }

    MappedFile *result = malloc(sizeof(MappedFile));
    result->length = (size_t) statistics.st_size;
    result->data = mappedFile;

    return result;
}

/// @return 0 on success,
/// non-zero value otherwise
int unmap(MappedFile *file)
{
    if (munmap(file->data, file->length) == -1)
    {
        perror("munmap");
        return 1;
    }
    return 0;
}
