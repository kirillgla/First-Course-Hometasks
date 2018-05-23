#include <stdio.h>
#include <math.h>

int isPrime(long long x)
{
    for (int i = 2; i < sqrt(x) + 1; i++) {
        if (x % i == 0)
        {
            return 0;
        }
    }
    return 1;
}

int main()
{
    printf("Mersenne primes:\n");
    long long src = 1;
    for (int i = 0; i < 31; i++)
    {
        src *= 2;
        if (isPrime(src - 1))
        {
            printf("%lli\n", src - 1);
        }
    }
    return 0;
}
