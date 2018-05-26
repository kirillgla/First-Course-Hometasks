#include <stdio.h>
#include <malloc.h>

#include "unsignedBignum.h"
#include "util.h"
#include "linkedList.h"

UBN *buildUBN(unsigned int value)
{
    UBN *result = buildLinkedList();
    pushValueToEnd(result, value);
    return result;
}

UBN *cloneUBN(UBN *ubn)
{
    if (ubn == NULL)
        return NULL;

    UBN *result = buildLinkedList();

    Element *current = ubn->first;

    while (current != NULL)
    {
        pushValueToEnd(result, current->value);
        current = current->next;
    }

    return result;
}

void freeUBN(UBN *ubn)
{
    freeList(ubn);
}

// private
/// UBN is assumed to be non-zero
/// Element is assumed to be non-null
void printHexUBNDigits(Element *current)
{
    if (current->next != NULL)
    {
        printHexUBNDigits(current->next);
        printf(HEX_FORMAT_FILL, current->value);
        return;
    }

    printf(HEX_FORMAT_FREE, current->value);
}

void printHexUBN(UBN *ubn)
{
    if (isZeroUBN(ubn))
    {
        printf("0");
        return;
    }
    printHexUBNDigits(ubn->first);
}

int isZeroUBN(UBN *ubn)
{
    if (ubn == NULL || ubn->first == NULL)
        return 1;

    Element *current = ubn->first;
    do
    {
        if (current->value != 0)
            return 0;
        current = current->next;
    }
    while (current != NULL);

    return 1;
}

void simplifyUBN(UBN *ubn)
{
    if (ubn == NULL || ubn->length == 0)
        return;

    Element *last = ubn->last;
    while (last->value == 0 && last->previous != NULL)
    {
        ubn->last = last->previous;
        last->previous->next = NULL;
        Element *tmp = last;
        last = last->previous;
        free(tmp);
    }
}

void addUBN(UBN *U, UBN *V)
{
    if (U == NULL || isZeroUBN(V))
        return;

    if (U->length == 0)
        pushValueToEnd(U, 0);

    // "int index" turns out to be redudant
    unsigned int carry = 0;
    Element *UCurrent = U->first;
    Element *VCurrent = V->first;

    while (1)
    {
        unsigned long long int sum = UCurrent->value + carry;
        if (VCurrent != NULL)
            sum += VCurrent->value;

        carry = (unsigned int) (sum / INT_MOD);
        UCurrent->value = (unsigned int) (sum % INT_MOD);

        if (carry == 0 && (VCurrent == NULL || VCurrent->next == NULL))
            break;

        if (UCurrent->next == NULL)
            pushValueToEnd(U, 0);
        UCurrent = UCurrent->next;
        VCurrent = VCurrent->next;
    }
}

void substractUBN(UBN *U, UBN *V)
{
    // We assume input to be valid

    signed int carry = 0;
    Element *UCurrent = U->first;
    Element *VCurrent = V->first;

    while (1)
    {
        signed long long int sum = UCurrent->value + carry;
        if (VCurrent != NULL)
            sum -= VCurrent->value;

        if (sum < 0)
        {
            carry = -1;
            sum += INT_MOD;
        }
        else
        {
            carry = 0;
        }

        // Dividing by INT_MOD is redudant here
        UCurrent->value = (unsigned int) sum;

        if (carry == 0 && (VCurrent == NULL || VCurrent->next == NULL))
            break;

        if (UCurrent->next == NULL)
            pushValueToEnd(U, 0);
        UCurrent = UCurrent->next;
        VCurrent = VCurrent->next;
    }
    simplifyUBN(U);
}

void leftShiftUBN(UBN *ubn, unsigned int ammount)
{
    if (isZeroUBN(ubn))
        return;

    for (int i = 0; i < ammount; i++)
    {
        Element *newElement = buildElement(0);
        newElement->next = ubn->first;
        ubn->first->previous = newElement;
        ubn->first = newElement;
    }
    ubn->length += ammount;
}

void rightShiftUBN(UBN *ubn, unsigned int ammount)
{
    int index = 0;
    while (index < ammount && ubn->length != 0)
    {
        free(popFirst(ubn));
        index++;
    }
    if (ubn->length == 0)
        pushValueToEnd(ubn, 0);
}

// private
/// Booth numbers are assumed
/// non-null and single-digit
void multiplySmallUBN(UBN *U, UBN *V)
{
    // No overflow can ever happen here
    unsigned long long int prod = (unsigned long long int) U->first->value * V->first->value;
    U->first->value = (unsigned int) (prod % INT_MOD);
    unsigned int carry = (unsigned int) (prod / INT_MOD);
    if (carry != 0)
        pushValueToEnd(U, carry);
}

/// Karatsuba algorithm
void multiplyUBN(UBN *U, UBN *V)
{
    if (isZeroUBN(U))
        return;

    if (isZeroUBN(V))
    {
        freeListContents(U);
        U->first = buildElement(0);
        U->last = U->first;
        U->length = 1;
        return;
    }

    if (U->length == 1 && V->length == 1)
    {
        multiplySmallUBN(U, V);
        return;
    }

    // Select base in which booth numbers
    // contain no more than 2 digits.
    // This is done by splitting the bigger number
    // into two roughly equal halves.
    // base = (2^32)^B, where B:
    unsigned int B = U->length > V->length ?
                     U->length / 2 + U->length % 2 :
                     V->length / 2 + V->length % 2;

    // U = x1 * base + x0
    UBN *x0 = splitListStart(U, B);
    UBN *x1 = U;
    // V = y1 * base + y0
    UBN *y0 = splitListStart(V, B);
    UBN *y1 = V;
    // W = UV = z2 * sqr(base) + z1 * base + z0
    // However, keeping z0 and z2 in memory
    // is redudant due to the possibility
    // of keeping z0 in x0 and z2 in x1
    UBN *z1;

    z1 = cloneUBN(x0);
    addUBN(z1, x1);
    // I have not found a way to avoid this variable
    UBN *tmp = cloneUBN(y0);
    addUBN(tmp, y1);
    multiplyUBN(z1, tmp);
    freeUBN(tmp);
    multiplyUBN(x0, y0);
    multiplyUBN(x1, y1);
    // Let's just hope nothing goes wrong...
    substractUBN(z1, x0);
    substractUBN(z1, x1);
    leftShiftUBN(x1, 2 * B);
    leftShiftUBN(z1, B);
    addUBN(x1, z1);
    addUBN(x1, x0);
    // Since x1 points to U,
    // U now stores the result of multiplication,
    // so it would be enough to repair V
    // and clean up a bit
    pushListToStart(y1, y0);
    // Since y1 points to V,
    // V now stores it's initial value
    freeUBN(x0);
    freeUBN(z1);
    freeUBN(y0);
    // return; That was fairly wired
}

void squareUBN(UBN *ubn)
{
    if (isZeroUBN(ubn))
        return;

    if (ubn->length == 1)
    {
        // No overflow can ever happen here
        unsigned long long int prod = (unsigned long long int) ubn->first->value * ubn->first->value;
        ubn->first->value = (unsigned int) (prod % INT_MOD);
        unsigned int carry = (unsigned int) (prod / INT_MOD);
        if (carry != 0)
            pushValueToEnd(ubn, carry);
        return;
    }

    // Main multiplication algorithm can be reduced to
    // (u1*B+u0)^2 = u1^2*B^2 + 2*u1*u2*B + u0^2
    // as it requires 1 multiplication
    // and 1 addition for calculating z1,
    // as opposed to 1 multiplication and 4 additions

    unsigned int B = ubn->length / 2 + ubn->length % 2;
    UBN *x0 = splitListStart(ubn, B);
    UBN *x1 = ubn;
    UBN *z1 = cloneUBN(x0);
    multiplyUBN(z1, x1);
    addUBN(z1, z1);
    squareUBN(x0);
    squareUBN(x1);
    leftShiftUBN(x1, 2 * B);
    leftShiftUBN(z1, B);
    addUBN(x1, z1);
    addUBN(x1, x0);
    // Since x1 points to @param ubn,
    // it now contains squaring result

    freeUBN(x0);
    freeUBN(z1);
}

void powerUBN(UBN *ubn, unsigned int power)
{
    if (power == 0)
    {
        freeListContents(ubn);
        ubn->first = buildElement(1);
        ubn->last = ubn->first;
        ubn->length = 1;
        return;
    }

    if (power == 1)
        return;

    if ((power & 1) == 0)
    {
        squareUBN(ubn);
        powerUBN(ubn, power >> 1);
        return;
    }
    // (power & 1) != 0
    UBN *mem = cloneUBN(ubn);
    squareUBN(ubn);
    powerUBN(ubn, power >> 1);
    multiplyUBN(ubn, mem);
//    do
//    {
//        if ((power & 1) != 0)
//            multiplyUBN(ubn, mem);
//        squareUBN(ubn);
//        power >>= 1;
//    }
//    while (power != 1);
}
