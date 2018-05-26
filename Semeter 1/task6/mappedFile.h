#ifndef TASK6_MAPPEDFILE_H
#define TASK6_MAPPEDFILE_H

typedef struct
{
    char *data;
    unsigned int length;
} MappedFile;

MappedFile *map(const char *, int, unsigned int);

int unmap(MappedFile *);

#endif //TASK6_MAPPEDFILE_H
