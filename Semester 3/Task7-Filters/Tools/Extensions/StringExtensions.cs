using System;
using System.Text;

namespace Tools.Extensions
{
    public static class StringExtensions
    {
        public static string Extend(this string s)
        {
            var result = new StringBuilder(256);
            result.Append(s);
            for (int index = 0; index < 256 - s.Length; index += 1)
            {
                result.Append(' ');
            }

            if (result.Length != 256)
            {
                throw new Exception("Internal error: extended string length is not 256");
            }

            return result.ToString();
        }
    }
}
