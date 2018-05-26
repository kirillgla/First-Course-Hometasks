#include <stdio.h>
#include <malloc.h>
#include <string.h>

char* name1 = "Kirill";
char* name2 = "Glazyrin";
char* name3 = "Maximovich";

int isLittleEndian()
{
    int value = 1;
    int *x = &value;
    short *y = (short*) x;
    return (int) *y;
}

void printBits(int length, int *bits)
{
    for (int i = length - 1; i >= 0; i--)
    {
        printf("%d", bits[i]);
        if (i % 8 == 0 && i != 0)
        {
            printf(" ");
        }
    }
}

void binaryInt(int x)
{
    int mem[sizeof(x) * 8];
    int bit = 1;
    for (int i = 0; i < sizeof(x) * 8; i++)
    {
        mem[i] = (x & bit) == 0? 0: 1;
        bit = bit << 1;
    }
    printBits(sizeof(x) * 8, mem);
}

void binaryLongLong(long long x)
{
    int mem[sizeof(x) * 8];
    long long bit = 1;
    for (int i = 0; i < sizeof(x) * 8; i++)
    {
        mem[i] = (x & bit) == 0? 0: 1;
        bit = bit << 1;
    }
    printBits(sizeof(x) * 8, mem);
}

// This case is a terrible one!
// It has to be dealt with separately.
void binaryDoubleX86(double *x)
{
    long long *ptr = (long long*) x;
    if (isLittleEndian())
    {
        binaryLongLong(*(ptr + 1));
        printf(" ");
        binaryLongLong(*ptr);
    }
    else
    {
        binaryLongLong(*ptr);
        printf(" ");
        binaryLongLong(*(ptr + 1));
    }
}

int main()
{
    if (sizeof(float) != 4 || sizeof(double) != 8)
    {
        printf("Error: unsupported machine.");
        return 1;
    }
    switch (sizeof(int))
    {
        case 2:
        {
            // x86
            long long x = - strlen(name1) * strlen(name2) * strlen(name3);
            printf("32-bit negative integer:\n%lli = \n", x);
            binaryLongLong(x);
            printf("\n");

            float *y = malloc(sizeof(float));
            *y = strlen(name1) * strlen(name2) * strlen(name3);
            printf("Positive single-precision floating-point number:\n%f = \n", *y);
            long long *ptr = (long long*) y;
            binaryLongLong(*ptr);
            printf("\n");
            free(y);

            double *z = malloc(sizeof(double));
            *z = - strlen(name1) * strlen(name2) * strlen(name3);
            printf("Negative double-precision floating-point number:\n%lf = \n", *z);
            binaryDoubleX86(z);
            printf("\n");
            free(z);

            return 0;
        }
        case 4:
        {
            // x32, x64
            int x = - strlen(name1) * strlen(name2) * strlen(name3);
            printf("32-bit negative integer:\n%d = \n", x);
            binaryInt(x);
            printf("\n");

            float *y = malloc(sizeof(float));
            *y = strlen(name1) * strlen(name2) * strlen(name3);
            printf("Positive single-precision floating-point number:\n%f = \n", *y);
            int *ptry = (int*) y;
            binaryInt(*ptry);
            printf("\n");
            free(y);

            double *z = malloc(sizeof(double));
            *z = - strlen(name1) * strlen(name2) * strlen(name3);
            printf("Negative double-precision floating-point number:\n%lf = \n", *z);
            long long *ptrz = (long long*) z;
            binaryLongLong(*ptrz);
            printf("\n");
            free(z);

            return 0;
        }
        default:
        {
            printf("Error: unsupported machine.");
            return 1;
        }
    }
}
