#ifndef TASK6_HARDCORE_STRINGARRAY_H
#define TASK6_HARDCORE_STRINGARRAY_H

typedef struct
{
    unsigned int length;
    char **data;
} StringArray;

void freeStringArray(StringArray *);

#endif /* TASK6_HARDCORE_STRINGARRAY_H */
