using System;

namespace Task1
{
    sealed class BasicImage
    {
        byte[] data;

        int width;

        int actualWidth;
        // Line length in bmp file is not equal to image width * 
        int height;
        uint offset;
        int bytesPerPixel;

        /// <summary>
        /// Accesses pixel at [i, j]
        /// </summary>
        /// <param name="i">row</param>
        /// <param name="j">column</param>
        /// <param name="part">part of colour accessed</param>
        /// <returns>Selected part of pixel at [i, j]</returns>
        internal byte this[int i, int j, ColourPart part]
        {
            get
            {
                if (part == ColourPart.Alpha && bytesPerPixel == 3)
                {
                    throw new ArgumentException("Error: attempt to read alpha channel which is not provided.");
                }
                if (i < 0 || i >= height || j < 0 || j >= width)
                {
                    return 0;
                }
                int start = (int)(offset + i * actualWidth + j * bytesPerPixel);
                int index = start + bytesPerPixel - (int)part - 1;
                if (index < 0 || index >= data.Length)
                {
                    throw new IndexOutOfRangeException($"Index out of range: i={i}, j={j}, part={part}");
                }
                return data[index];
            }
            set
            {
                if (part == ColourPart.Alpha && bytesPerPixel == 3)
                {
                    throw new ArgumentException("Error: attempt to write to alpha channel which is not provided.");
                }
                if (i < 0 || i >= height || j < 0 || j >= width)
                {
                    throw new ArgumentOutOfRangeException($"Error: index ({i}, {j}, {part}) is out of range");
                }
                int start = (int)(offset + i * actualWidth + j * bytesPerPixel);
                int index = start + bytesPerPixel - (int)part - 1;
                data[index] = value;
            }
        }
        
        public BasicImage(byte[] data, BitMapFileHeader fileHeader, BitMapInfoHeader infoHeader)
        {
            Util.CheckSizes(fileHeader, infoHeader, data.Length);
            width = infoHeader.BiWidth;
            actualWidth = ((infoHeader.BiBitCount * width + 31) / 32) * 4;
            height = infoHeader.BiHeight;
            offset = fileHeader.BfOffBits;
            bytesPerPixel = infoHeader.BiBitCount / 8;
            this.data = data;
        }
    }
}
