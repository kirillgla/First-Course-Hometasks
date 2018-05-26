#include <stdio.h>
#include "unsignedBignum.h"

int main()
{
    UBN *ubn = buildUBN(3);
    const unsigned int power = 5000;
    powerUBN(ubn, power);
    printHexUBN(ubn);
    printf("\n");
    freeUBN(ubn);
    return 0;
}
