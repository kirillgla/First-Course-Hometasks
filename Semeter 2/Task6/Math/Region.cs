using System;

namespace Math
{
    public struct Region
    {
        public readonly Point UpperLeft;
        public readonly Point LoweRight;

        public double Width => LoweRight.Y - UpperLeft.X;

        public double Height => LoweRight.X - UpperLeft.X;

        public Region(Point upperLeft, Point loweRight)
        {
            if (upperLeft.Y >= loweRight.Y || upperLeft.X >= loweRight.X)
            {
                throw new ArgumentException(
                    "upper-left corner should actually be upper-left relatively to lower-right one");
            }

            UpperLeft = upperLeft;
            LoweRight = loweRight;
        }
    }
}
