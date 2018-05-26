#include <stdio.h>
#include <time.h>
#include <malloc.h>

#include "util.h"

#define COIN_TYPES 8
#define COIN_MAX 200

const int amounts[COIN_TYPES] = {1, 2, 5, 10, 20, 50, 100, COIN_MAX};

unsigned long long int **buildMemoizationStructure(unsigned int length)
{
    unsigned long long int **result = (unsigned long long int **) malloc(sizeof(unsigned long long int *) * length);
    for (int i = 0; i < length; i++)
    {
        result[i] = calloc(COIN_TYPES, sizeof(unsigned long long int));
    }
    return result;
}

void freeMemoizationStructure(unsigned long long int **structure, unsigned int length)
{
    for (int i = 0; i < length; i++)
    {
        free(structure[i]);
    }
    free(structure);
}

/// not to be called out of getOptionsDynamicFull
unsigned long long int getOptionsDynamicRecursion(
        unsigned long long int **mem, unsigned int sum, unsigned int currentCoinIndex)
{
    if (mem[sum][currentCoinIndex])
        return mem[sum][currentCoinIndex];

    if (sum == 0)
        return 1;

    if (currentCoinIndex == 0)
        return 1;

    unsigned long long int result = 0;
    signed long long int newSum;
    for (unsigned int currentCoinsAdded = 0;
         (newSum = (signed long long int) sum - currentCoinsAdded * amounts[currentCoinIndex]) >= 0;
         currentCoinsAdded++)
    {
        result += getOptionsDynamicRecursion(mem, (unsigned int) newSum, currentCoinIndex - 1);
    }
    mem[sum][currentCoinIndex] = result;
    return result;
}

unsigned long long int getOptionsDynamic(unsigned int sum)
{
    unsigned long long int **mem = buildMemoizationStructure(sum + 1);

    if (mem == NULL)
        return 0;

    unsigned long long int result = getOptionsDynamicRecursion(mem, sum, COIN_TYPES - 1);

    freeMemoizationStructure(mem, sum);
    return result;
}

int main()
{
    // introduction by Bashkirov Alexandr
    printf(
            "#---------------------------- BEGIN OF USAGE ----------------------------#\n"
                    "| Program gets an amount of money in pence and prints a number of ways   |\n"
                    "| in which you can take it by using just an any amount of any English    |\n"
                    "| coins:                                                                 |\n"
                    "|  *   1 pence                                                           |\n"
                    "|  *   2 pence                                                           |\n"
                    "|  *   5 pence                                                           |\n"
                    "|  *  10 pence                                                           |\n"
                    "|  *  20 pence                                                           |\n"
                    "|  *  50 pence                                                           |\n"
                    "|  * 100 pence (1 pound)                                                 |\n"
                    "|  * 200 pence (2 pounds)                                                |\n"
                    "#---------------------------- END OF USAGE ------------------------------#\n\n"
    );
    unsigned int sum = (unsigned int) readInput();

    clock_t start = clock();
    printf("There are %llu ways to present %d pence. ", getOptionsDynamic(sum), sum);
    clock_t end = clock();
    printf("(%ld ticks)\n", (end - start));

    return 0;
}
