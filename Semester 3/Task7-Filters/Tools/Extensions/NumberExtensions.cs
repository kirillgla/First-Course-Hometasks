using System;

namespace Tools.Extensions
{
    public static class NumberExtensions
    {
        public static bool IsPerfectSquare(this int number)
        {
            int root = (int) Math.Floor(Math.Sqrt(number));
            return root.Sqr() == number;
        }

        public static int IntegralRoot(this int number)
        {
            if (!IsPerfectSquare(number))
            {
                throw new InvalidOperationException();
            }

            int root = (int) Math.Floor(Math.Sqrt(number));
            return root;
        }

        public static int Sqr(this int number) => number * number;

        public static byte ToByte(this double value)
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

        public static byte AbsToByte(this double value)
        {
            if (Math.Abs((int) value) > 255)
            {
                return 255;
            }

            return (byte) Math.Abs((int) value);
        }

        public static string ReadableSize(this int source)
        {
            double size = source;
            if (size < 0)
            {
                return "Negative size";
            }
            // This aint ever gonna fail
            string[] suffixes = {"b", "Kb", "Mb", "Gb", "Tb", "Yb"};
            const int factor = 1024;
            int index = 0;
            for (; size >= factor; index++)
            {
                size /= factor;
            }

            return $"{size:n}{suffixes[index]}";
        }
    }
}
