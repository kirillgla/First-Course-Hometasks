#include <stdio.h>
#include <time.h>

#include "util.h"

#define COIN_TYPES 8
#define COIN_MAX 200
const int ammounts[COIN_TYPES] = {1, 2, 5, 10, 20, 50, 100, COIN_MAX};

unsigned long long int getOptionsRecursion(
        unsigned long long int currentSum, unsigned int currentCoinIndex, unsigned int limit)
{
    if (currentSum == limit)
        return 1ull;

    if (currentCoinIndex == 0)
    {
        return 1;
        /*
        Following would allow to support cases with ammounts[0] != 1:
        if ((limit - currentSum) % ammounts[0] == 0)
            return 1;
        else
            return 0;
        */
    }

    unsigned long long int result = 0ull;
    unsigned long long int newSum;

    for (int currentCoinsAdded = 0;
         (newSum = currentSum + currentCoinsAdded * ammounts[currentCoinIndex]) <= limit;
         currentCoinsAdded++)
    {
        result += getOptionsRecursion(newSum, currentCoinIndex - 1, limit);
    }
    return result;
}

/// Same recursive approach as getOptionsRecursion
/// but adapted for applying dynamic programming approach
/// @param currentCoinIndex index of coin currently being collected
///     coins of greated index can't be used.
/// @param sum total ammount left to collect using coins mentioned
unsigned long long int getOptionsOptimizedRecursion(unsigned int sum, unsigned int currentCoinIndex)
{
    if (sum == 0)
        return 1ull;

    if (currentCoinIndex == 0)
        // same as in getOptionsRecursion
        return 1;

    unsigned long long int result = 0ull;
    signed int newSum;

    for (unsigned int currentCoinsAdded = 0;
         (newSum = sum - currentCoinsAdded * ammounts[currentCoinIndex]) >= 0;
         currentCoinsAdded++)
    {
        // newSum has been checked to be positive, so it can safely be casted
        result += getOptionsOptimizedRecursion((unsigned int) newSum, currentCoinIndex - 1);
    }
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
    unsigned int ammount = (unsigned int) readInput();

    clock_t start = clock();
    printf("There are %llu ways to present %d pence. ", getOptionsOptimizedRecursion(ammount, COIN_TYPES - 1), ammount);
    clock_t end = clock();
    printf("(%ld ticks)\n", (end - start));

    start = clock();
    printf("There are %llu ways to present %d pence. ", getOptionsRecursion(0ull, COIN_TYPES - 1, ammount), ammount);
    end = clock();
    printf("(%ld ticks)\n", (end - start));

    return 0;
}
