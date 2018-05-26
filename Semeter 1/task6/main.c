#include <stdio.h>
#include <fcntl.h>

#include "mstring.h"

int main(int argc, char *argv[])
{
    // ==== ==== Manage input ==== ====
    if (argc != 2)
    {
        fprintf(stderr, "usage: %s <filename>\n", argv[0]);
        return 1;
    }

    MappedFile *file = map(argv[1], O_RDWR, 0);
    if (file == NULL)
        return 1;

    // ==== ==== Interact with file ==== ====

    if (sort(file, argv[0]))
        return 1;

    // ==== ==== Finish and clen up ==== ====

    if (unmap(file))
        return 1;

    return 0;
}
