#ifndef UTIL_H
#define UTIL_H

#include <stdio.h>
#include <inttypes.h>

#define BIG_ENDIAN 1
#define LITTLE_ENDIAN 2

// Should use sizeof(BITMAPFILEHEADER) instead
// #define BITMAP_FILE_HEADER_SIZE 14

#define LUMINANCE_RED 0.2126
#define LUMINANCE_GREEN 0.7152
#define LUMINANCE_BLUE 0.0722

#define FILTER_ASSERT(condition, message) if (!(condition)) {printf(message); free(previous); free(current); free(next); if (gap != 0) free(gapBuffer); return 1;}

#pragma pack(push, 1)

// All ugly variable names are equal to the ones in documentation.
typedef struct
{
    uint16_t bfType; // File format and system endianness
    uint32_t bfSize; // File size
    uint16_t bfReserved1; // Reserved field, needs to be 0
    uint16_t bfReserved2; // Reserved field, needs to be 0
    uint32_t bfOffBits; // Pixel data offset
} BITMAPFILEHEADER;

typedef struct
{
    uint32_t biSize; // Size of bitmap-info-header structure
    int32_t biWidth; // Image width
    int32_t biHeight; // Image height
    uint16_t biPlains; // The number of color planes (must be 1)
    uint16_t biBitCount; // Number of bits per pixel
} BITMAPINFOHEADER;

#pragma pack(pop)

const char *gauss;
const char *sobelx;
const char *sobely;
const char *greyen;

unsigned char toByte(double);

unsigned char absToByte(double);

/// Returns whether user approves requested action or not.
int confirm(char*);

int exists(const char*, const char*);

/// Returns 0 on success, non-zero value otherwise
/// type[0] is supposed be '-'.
int choose(const char*, char*, char**, char**, char**);

/// Returns 0 on success, non-zero value otherwise
int handleArguments(int, char**, char**, char**, char**);

/// Returns 0 on success, non-zero value otherwise
int handleBitmapFileHeader(FILE*, BITMAPFILEHEADER*, int*);

/// BITMAPCOREHEADER is not supported any longer
/// Returns 0 on success, non-zero value otherwise
int handleBitmapInfoHeader(FILE*, BITMAPINFOHEADER*);

/// Returns 0 on success, non-zero value otherwise
int checkSizes(BITMAPFILEHEADER*, BITMAPINFOHEADER*);

/// Returns 0 on success, non-zero value otherwise
int copyHeader(FILE*, FILE*, uint32_t);

// Following two methods could have been implemented in a different way
// by creating a single modify() method that accepts function pointer as an argument.

/// Returns 0 on success, non-zero value otherwise
int applyKernel(BITMAPINFOHEADER*, const double[3][3], FILE*, FILE*, unsigned char (*)(double));

int applyGreyen(BITMAPINFOHEADER*, FILE*, FILE*, int);

#endif /* UTIL_H */
