#include <malloc.h>

#include "stringArray.h"

void freeStringArray(StringArray *array)
{
    if (array == NULL)
        return;

    for (unsigned int i = 0; i < array->length; i++)
        free(array->data[i]);

    if (array->length != 0)
        free(array->data);

    free(array);
}
