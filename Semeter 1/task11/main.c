#include <stdio.h>
#include <malloc.h>

const unsigned int start = 2;
const unsigned int end = 999999;

/// Digital Root
unsigned int DR(unsigned long long int x)
{
    unsigned int mod = (unsigned int) (x % 9);
    return mod ? mod : 9;
}

/*
 * MDRS can be calculated
 * using previously calculated values:
 * MDRS(n) = max{MDRS(a) + MDRS(n / a) | a in N && a divides n}
 */

int main()
{
    // MDRS[0] and MDRS[1] are unused.
    // Well, we can afford to waste 8 bytes
    // so that to have beautiful interface
    unsigned int *MDRS = (unsigned int *) malloc(sizeof(unsigned int) * end + 1);

    // All single-digit numbers can be initialized manually
    for (unsigned int i = start; i < 10; i++)
        MDRS[i] = i;

    // Calculate MDRS for all the other values
    for (unsigned int i = 10; i <= end; i++)
    {
        MDRS[i] = DR(i);
        for (unsigned int j = 2; j * j <= i; j++)
        {
            unsigned int sum;
            if (i % j == 0 && (sum = MDRS[j] + MDRS[i / j]) > MDRS[i])
            {
                MDRS[i] = sum;
            }
        }
    }

    // MDRS sum
    unsigned long long int result = 0ull;
    for (unsigned int i = start; i <= end; i++)
    {
        result += MDRS[i];
    }

    printf("Sum of MDRS(n) for n in 2:999999 = %llu\n", result);

    free(MDRS);
    return 0;
}
