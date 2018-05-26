#include <stdio.h>
#include <ctype.h>

#include "util.h"

int readInput()
{
    printf("Please, enter a number: ");
    while (1)
    {
        int input;
        char after; // Used to ensure that input is integer.
        int result = scanf("%d%c", &input, &after); // NOLINT
        if (result == 2 && input > 0 && isspace(after))
        {
            if (after != '\n')
            {
                while (getchar() != '\n'); // We need to free the input stream
            }
            return input;
        }
        else if (result != 2 || !isspace(after))
        {
            while (getchar() != '\n'); // We need to free the input stream
        }
        printf("Error! Please, enter a valid number: ");
    }
}
