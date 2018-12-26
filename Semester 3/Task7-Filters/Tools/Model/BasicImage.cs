using System;
using System.Collections.Generic;
using Tools.Extensions;
using Tools.Functional;

namespace Tools.Model
{
    public sealed class BasicImage
    {
        // public IReadOnlyList<byte> Contents => Data;
        // public for performance considerations
        public byte[] Data { get; }

        int Width { get; }

        int ActualWidth { get; }

        // Line length in bmp file is not equal to image width * 
        int Height { get; }
        uint Offset { get; }
        int BytesPerPixel { get; }

        /// <summary>
        /// Accesses pixel at [i, j]
        /// </summary>
        /// <param name="i">row</param>
        /// <param name="j">column</param>
        /// <param name="part">part of colour accessed</param>
        /// <returns>Selected part of pixel at [i, j]</returns>
        byte this[int i, int j, ColourPart part]
        {
            get
            {
                if (part == ColourPart.Alpha && BytesPerPixel == 3)
                {
                    throw new ArgumentException("Error: attempt to read alpha channel which is not provided.");
                }

                if (i < 0 || i >= Height || j < 0 || j >= Width)
                {
                    return 0;
                }

                int start = (int) (Offset + i * ActualWidth + j * BytesPerPixel);
                int index = start + BytesPerPixel - (int) part - 1;
                if (index < 0 || index >= Data.Length)
                {
                    throw new IndexOutOfRangeException($"Index out of range: i={i}, j={j}, part={part}");
                }

                return Data[index];
            }
            set
            {
                if (part == ColourPart.Alpha && BytesPerPixel == 3)
                {
                    throw new ArgumentException("Error: attempt to write to alpha channel which is not provided.");
                }

                if (i < 0 || i >= Height || j < 0 || j >= Width)
                {
                    throw new ArgumentOutOfRangeException($"Error: index ({i}, {j}, {part}) is out of range");
                }

                int start = (int) (Offset + i * ActualWidth + j * BytesPerPixel);
                int index = start + BytesPerPixel - (int) part - 1;
                Data[index] = value;
            }
        }

        // Yeah, this constructor throws sometimes. I know that this sucks
        public BasicImage(byte[] data) : this(data, Utils.FindBitMapFileHeader(data), Utils.FindBitMapInfoHeader(data))
        {
        }

        BasicImage(byte[] data, BitMapFileHeader fileHeader, BitMapInfoHeader infoHeader)
        {
            if (infoHeader.BiBitCount != 24 && infoHeader.BiBitCount != 32)
            {
                throw new ArgumentException("Error: number of bits per pixel other than 24 and 32 are not supported.");
            }

            if (fileHeader.BfSize != data.Length ||
                infoHeader.BiWidth * infoHeader.BiHeight * (infoHeader.BiBitCount / 8) > fileHeader.BfSize)
            {
                throw new ArgumentException("Error: file contents don't match declared size.");
            }

            Width = infoHeader.BiWidth;
            ActualWidth = (infoHeader.BiBitCount * Width + 31) / 32 * 4;
            Height = infoHeader.BiHeight;
            Offset = fileHeader.BfOffBits;
            BytesPerPixel = infoHeader.BiBitCount / 8;
            Data = data;
        }

        BasicImage(byte[] data, int width, int actualWidth, int height, uint offset, int bytesPerPixel)
        {
            Data = data;
            Width = width;
            ActualWidth = actualWidth;
            Height = height;
            Offset = offset;
            BytesPerPixel = bytesPerPixel;
        }

        BasicImage Clone() => new BasicImage(Data, Width, ActualWidth, Height, Offset, BytesPerPixel);

        public Maybe<BasicImage> WithKernelApplied(
            double[][] kernel,
            Action<double> onProgressChanged,
            LightSwitch continuation
        )
        {
            var result = Clone();
            var lastPart = BytesPerPixel == 3 ? ColourPart.Blue : ColourPart.Alpha;

            var checkpoint = DateTime.Now;
            for (int index = 0; index < Height; index++)
            {
                for (int jndex = 0; jndex < Width; jndex++)
                {
                    for (var part = ColourPart.Red; part <= lastPart; part++)
                    {
                        var address = new ImageAddress(index, jndex, part);
                        result[index, jndex, part] = ComputeNewPixelValue(kernel, address).AbsToByte();
                        if (!continuation.IsActive)
                        {
                            return new Nothing<BasicImage>();
                        }
                    }

                    checkpoint = ReportProgress(onProgressChanged, checkpoint, index, jndex);
                }
            }

            onProgressChanged(100);
            return new Just<BasicImage>(result);
        }

        DateTime ReportProgress(Action<double> onProgressChanged, DateTime checkpoint, int index, int jndex)
        {
            var now = DateTime.Now;
            var elapsed = now - checkpoint;
            if (elapsed.Seconds < 1)
            {
                return checkpoint;
            }

            onProgressChanged(GetCurrentProgress(index, jndex));
            return now;
        }

        double GetCurrentProgress(int index, int jndex)
        {
            int total = Width * Height;
            int done = index * Width + jndex;
            return 100.0 * done / total;
        }

        double ComputeNewPixelValue(IReadOnlyList<IReadOnlyList<double>> kernel, ImageAddress address)
        {
            double result = 0;
            const int kernelSize = 3;
            for (int row = 0; row < kernelSize; row++)
            {
                for (int column = 0; column < kernelSize; column++)
                {
                    result += kernel[row][column] *
                              this[
                                  address.Index - 1 + row,
                                  address.Jndex - 1 + column,
                                  address.ColourPart
                              ];
                }
            }

            return result;
        }
    }
}
