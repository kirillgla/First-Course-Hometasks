using System;
using System.IO;

namespace Task1
{
    enum ColourPart
    {
        Red = 0,
        Green = 1,
        Blue = 2,
        Alpha = 3
    }

    // All ugly variable names are equal to the ones in documentation.

    struct BitMapFileHeader
    {
        internal uint BfSize; // File size
        internal uint BfOffBits; // Pixel data offset
    }

    struct BitMapInfoHeader
    {
        internal int BiWidth; // Image width
        internal int BiHeight; // Image height
        internal ushort BiBitCount; // Number of bits per pixel
    }

    static class Util
    {
        const double RedLuminance = 0.2126;
        const double GreenLuminance = 0.7152;
        const double BlueLuminance = 0.0722;

        static byte ToByte(double value)
        {
            if (value < 0)
            {
                return 0;
            }

            if (value > 255)
            {
                return 255;
            }

            return (byte) value;
        }

        static byte AbsToByte(double value)
        {
            if (Math.Abs((int) value) > 255)
            {
                return 255;
            }

            return (byte) Math.Abs((int) value);
        }

        /// type[0] is supposed be '-'
        /// length of type is supposed to be 2
        static void Choose(string type, string value, ref string source, ref string filter, ref string destination)
        {
            switch (type[1])
            {
                case 'i':
                    if (source != null)
                    {
                        throw new ArgumentException("Error: -i parameter provided more than once.");
                    }

                    source = value;
                    return;
                case 'f':
                    if (filter != null)
                    {
                        throw new ArgumentException("Error: -f parameter provided more than once.");
                    }

                    filter = value;
                    return;
                case 'o':
                    if (destination != null)
                    {
                        throw new ArgumentException("Error: -o parameter provided more than once.");
                    }

                    destination = value;
                    return;
                default:
                    throw new ArgumentException($"Error: \"{type}\" is unknown argument specifier.");
            }
        }

        internal static void HandleArguments(string[] args, out string source, out string filter,
            out string destination)
        {
            source = null;
            filter = null;
            destination = null;

            int i = 0;
            while (i < args.Length)
            {
                if (args[i][0] != '-')
                {
                    throw new ArgumentException($"Error: argument \"{args[i]}\" provided without type specifier.");
                }

                if (args[i].Length != 2)
                {
                    throw new ArgumentException($"Error: {args[i]} is not a valid type specifier.");
                }

                if (i + 1 == args.Length || args[i + 1][0] == '-')
                {
                    throw new ArgumentException($"Error: no value is provided after \"{args[i]}\".");
                }

                Choose(args[i], args[i + 1], ref source, ref filter, ref destination);
                i += 2;
            }

            if (source == null)
            {
                throw new ArgumentException("Error: source file not provided.");
            }

            if (destination == null)
            {
                throw new ArgumentException("Error: destination file not provided.");
            }

            if (filter == null)
            {
                throw new ArgumentException("Error: filter type not provided.");
            }

            Console.WriteLine($"Selected filter: \"{filter}\"");
            Console.WriteLine($"Source file path: \"{source}\"");
            Console.WriteLine($"Destination file path: \"{destination}\"");

            if (!File.Exists(source))
            {
                throw new ArgumentException("Error: source file doesn't exist.");
            }

            if (!File.Exists(destination))
            {
                return;
            }

            if (source == destination)
            {
                throw new ArgumentException("Error: destination file path is equal to source file path.");
            }
        }

        internal static void HandleBitMapFileHeader(byte[] bytes, out BitMapFileHeader header)
        {
            header = new BitMapFileHeader();
            if (bytes[0] == 0x42 && bytes[1] == 0x4D)
            {
                header.BfSize = (uint) ((bytes[5] << 24) + (bytes[4] << 16) + (bytes[3] << 8) + bytes[2]);
                header.BfOffBits = (uint) ((bytes[13] << 24) + (bytes[12] << 16) + (bytes[11] << 8) + bytes[10]);
            }
            else if (bytes[0] == 0x4D && bytes[1] == 0x42)
            {
                throw new ArgumentException("Big-endian images are not supported.");
            }
            else
            {
                throw new ArgumentException("Source is not a valid bmp file.");
            }
        }

        internal static void HandleBitMapInfoHeader(byte[] bytes, out BitMapInfoHeader infoHeader)
        {
            infoHeader = new BitMapInfoHeader
            {
                BiWidth = (bytes[21] << 24) + (bytes[20] << 16) + (bytes[19] << 8) + bytes[18],
                BiHeight = (bytes[25] << 24) + (bytes[24] << 16) + (bytes[23] << 8) + bytes[22],
                BiBitCount = (ushort) ((bytes[29] << 8) + (bytes[28]))
            };
        }

        internal static void CheckSizes(BitMapFileHeader fileHeader, BitMapInfoHeader infoHeader, int actualSize)
        {
            if (infoHeader.BiBitCount != 24 && infoHeader.BiBitCount != 32)
            {
                throw new ArgumentException("Error: nuber of bits per pixel other than 24 and 32 are not supported.");
            }

            if (fileHeader.BfSize != actualSize ||
                infoHeader.BiWidth * infoHeader.BiHeight * (infoHeader.BiBitCount / 8) > fileHeader.BfSize)
            {
                throw new ArgumentException("Error: file contents don't match declared size.");
            }
        }

        internal static void CopyHeader(byte[] source, byte[] destination, uint headerSize)
        {
            Array.Copy(source, destination, headerSize);
        }

        internal static void ApplyKernel(
            byte[] source,
            byte[] destination,
            double[][] kernel,
            BitMapFileHeader fileHeader,
            BitMapInfoHeader infoHeader)
        {
            var sourceImage = new BasicImage(source, fileHeader, infoHeader);
            var destinationImage = new BasicImage(destination, fileHeader, infoHeader);
            var lastPart = ColourPart.Alpha;
            if (infoHeader.BiBitCount == 24)
            {
                lastPart = ColourPart.Blue;
            }

            for (int i = 0; i < infoHeader.BiHeight; i++)
            {
                for (int j = 0; j < infoHeader.BiWidth; j++)
                {
                    for (var part = ColourPart.Red; part <= lastPart; part++)
                    {
                        double value = 0;
                        for (int row = 0; row < 3; row++)
                        {
                            for (int column = 0; column < 3; column++)
                            {
                                value += kernel[row][column] * sourceImage[i - 1 + row, j - 1 + column, part];
                            }
                        }

                        destinationImage[i, j, part] = AbsToByte(value);
                    }
                }
            }
        }

        internal static void ApplyGreyen(
            byte[] source,
            byte[] destination,
            BitMapFileHeader fileHeader,
            BitMapInfoHeader infoHeader)
        {
            var sourceImage = new BasicImage(source, fileHeader, infoHeader);
            var destinationImage = new BasicImage(destination, fileHeader, infoHeader);
            for (int i = 0; i < infoHeader.BiHeight; i++)
            {
                for (int j = 0; j < infoHeader.BiWidth; j++)
                {
                    byte average = ToByte(sourceImage[i, j, ColourPart.Red] * RedLuminance
                                          + sourceImage[i, j, ColourPart.Green] * GreenLuminance
                                          + sourceImage[i, j, ColourPart.Blue] * BlueLuminance);
                    destinationImage[i, j, ColourPart.Red] = average;
                    destinationImage[i, j, ColourPart.Green] = average;
                    destinationImage[i, j, ColourPart.Blue] = average;
                }
            }
        }
    }
}
