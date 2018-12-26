namespace Tools.Model
{
    public struct BitMapFileHeader
    {
        public BitMapFileHeader(uint bfSize, uint bfOffBits) =>
            (BfSize, BfOffBits) = (bfSize, bfOffBits);

        public uint BfSize { get; } // File size
        public uint BfOffBits { get; } // Pixel data offset
    }
}
