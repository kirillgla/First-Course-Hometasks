using System;
using System.Linq;

namespace Tools.Extensions
{
    public static class ArrayExtensions
    {
        public static T[] ParseMany<T>(this string str, Func<string, T> parser) =>
            str.Split(';').Select(parser).ToArray();

        public static T[] TryParseMany<T>(this string str, Func<string, T> parser)
        {
            try
            {
                return str.ParseMany(parser);
            }
            catch (FormatException)
            {
                return null;
            }
        }

        public static T[][] BuildMatrix<T>(this T[] source, int rows, int columns)
        {
            int size = source.Length;
            if (rows * columns != size)
            {
                throw new ArgumentException($"Bad size: rows*columns = {rows}*{columns} != {size} = size");
            }

            T[][] result = new T[rows][];

            for (int index = 0; index < rows; index += 1)
            {
                result[index] = new T[columns];
                for (int jndex = 0; jndex < columns; jndex += 1)
                {
                    result[index][jndex] = source[index * columns + jndex];
                }
            }

            return result;
        }

        // 1. Median :: Ord t => [t] -> t looks much better imo
        // 2. I know this can be done way faster
        public static T Median<T>(this T[] array) where T : IComparable<T>
        {
            int length = array.Length;
            if (length == 0)
            {
                throw new InvalidOperationException("Attempting to take a median of empty array");
            }
            
            Array.Sort(array);

            int mid = length / 2;
            return array[mid];
        }
    }
}
