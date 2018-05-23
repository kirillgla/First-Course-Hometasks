#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "util.h"

const char *about = "This is an utility that applies selected filter to selected .bmp image.\n"
                    "Arguments:\n"
                    "-i: path to file to be modified.\n"
                    "-o: path to desired destination of resulting image.\n"
                    "-f: filter to be applied.\n"
                    "  possible filters:\n"
                    "    gauss\n"
                    "    sobelx\n"
                    "    sobely\n"
                    "    greyen\n";

const double gaussMatrix[3][3] = {{1.0 / 16, 1.0 / 8, 1.0 / 16},
                                  {1.0 / 8, 1.0 / 4, 1.0 / 8},
                                  {1.0 / 16, 1.0 / 8, 1.0 / 16}};

// I used normalisation so that to make sure
// resulting image is no brighter than the original one.

const double sobelxMatrix[3][3] = {{-3.0 / 32, 0, 3.0 / 32},
                                   {-10.0 / 32, 0, 10.0 / 32},
                                   {-3.0 / 32, 0, 3.0 / 32}};

const double sobelyMatrix[3][3] = {{-3.0 / 32, -10.0 / 32, -3.0 / 32},
                                   {0, 0, 0},
                                   {3.0 / 32, 10.0 / 32, 3.0 / 32}};

int main(int argc, char **argv)
{
    if (argc == 1)
    {
        printf(about);
        return 0;
    }

    char *source; // Argument passed after -i, source file path
    char *filter; // Argument passed after -f, filter type
    char *destination; // Argument passed after -o, destination file path

    if (handleArguments(argc, argv, &source, &filter, &destination))
        return 1;

    FILE *fileStreamIn = fopen(source, "rb");
    FILE *fileStreamOut = fopen(destination, "wb");

    BITMAPFILEHEADER fileHeader;

    BITMAPINFOHEADER infoHeader;

    int platform;

    // If it isn't beautiful, huh?
    if (
            handleBitmapFileHeader(fileStreamIn, &fileHeader, &platform) ||
            handleBitmapInfoHeader(fileStreamIn, &infoHeader) ||
            checkSizes(&fileHeader, &infoHeader) ||
            // I know this is a bad thing to do, but I use fseek just once, what can possibly go wrong?
            fseek(fileStreamIn, 0, SEEK_SET) ||
            copyHeader(fileStreamIn, fileStreamOut, fileHeader.bfOffBits) ||
            printf("Working...\n") < 0 ||
            strcmp(filter, gauss) == 0 && applyKernel(&infoHeader, gaussMatrix, fileStreamIn, fileStreamOut, &toByte) ||
            // Note: it is not clear whether I should greyen image when applying sobel filter ir not.
            strcmp(filter, sobelx) == 0 && applyKernel(&infoHeader, sobelxMatrix, fileStreamIn, fileStreamOut, &absToByte) ||
            strcmp(filter, sobely) == 0 && applyKernel(&infoHeader, sobelyMatrix, fileStreamIn, fileStreamOut, &absToByte) ||
            strcmp(filter, greyen) == 0 && applyGreyen(&infoHeader, fileStreamIn, fileStreamOut, platform)
    )
    {
        fclose(fileStreamIn);
        fclose(fileStreamOut);
        return 1;
    }

    fclose(fileStreamIn);
    fclose(fileStreamOut);
    printf("Done.\n");
    return 0;
}
