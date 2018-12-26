using System;

namespace Tools.Model
{
    public static class Utils
    {
        internal static BitMapFileHeader FindBitMapFileHeader(byte[] bytes)
        {
            if (bytes[0] == 0x42 && bytes[1] == 0x4D)
            {
                uint size = (uint) ((bytes[5] << 24) + (bytes[4] << 16) + (bytes[3] << 8) + bytes[2]);
                uint offset = (uint) ((bytes[13] << 24) + (bytes[12] << 16) + (bytes[11] << 8) + bytes[10]);
                return new BitMapFileHeader(size, offset);
            }

            if (bytes[0] == 0x4D && bytes[1] == 0x42)
            {
                throw new ArgumentException("Big-endian images are not supported.");
            }

            throw new ArgumentException("Source is not a valid bmp file.");
        }

        internal static BitMapInfoHeader FindBitMapInfoHeader(byte[] bytes)
        {
            int width = (bytes[21] << 24) + (bytes[20] << 16) + (bytes[19] << 8) + bytes[18];
            int height = (bytes[25] << 24) + (bytes[24] << 16) + (bytes[23] << 8) + bytes[22];
            ushort bitCount = (ushort) ((bytes[29] << 8) + bytes[28]);
            return new BitMapInfoHeader(width, height, bitCount);
        }
    }
}
