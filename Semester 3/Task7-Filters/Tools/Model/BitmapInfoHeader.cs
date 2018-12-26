namespace Tools.Model
{
    public struct BitMapInfoHeader
    {
        public BitMapInfoHeader(int biWidth, int biHeight, ushort biBitCount) =>
            (BiWidth, BiHeight, BiBitCount) = (biWidth, biHeight, biBitCount);

        public int BiWidth { get; } // Image width
        public int BiHeight { get; } // Image height
        public ushort BiBitCount { get; } // Number of bits per pixel
    }
}
