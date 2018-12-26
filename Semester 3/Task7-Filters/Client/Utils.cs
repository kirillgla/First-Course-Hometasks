using System;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using Tools.Extensions;

namespace Client
{
    public static class Utils
    {
        public static async Task<string[]> ReadFiltersAsync(Stream stream)
        {
            byte filtersNumber = await stream.ForceReadByteAsync();
            string[] result = new string[filtersNumber];
            const int stringLength = 256;
            byte[] buffer = new byte[stringLength];
            for (int index = 0; index < filtersNumber; index += 1)
            {
                await stream.ForceReadAsync(buffer, 0, stringLength);
                result[index] = Encoding.ASCII.GetString(buffer).Trim();
            }

            return result;
        }

        public static string[] ReadFiltersSynchronously(Stream stream) => ReadFiltersAsync(stream).Result;

        [Obsolete("Async version should be used")]
        public static string[] ReadFilters(Stream stream)
        {
            byte filtersNumber = ReadSize(stream);
            string[] result = new string[filtersNumber];
            const int stringLength = 256;
            byte[] buffer = new byte[stringLength];
            for (int index = 0; index < filtersNumber; index += 1)
            {
                int bytesRead = stream.Read(buffer, 0, stringLength);
                if (bytesRead != stringLength)
                {
                    throw new EndOfStreamException();
                }

                result[index] = Encoding.ASCII.GetString(buffer).Trim();
            }

            return result;
        }

        [Obsolete("Async version should be used")]
        static byte ReadSize(Stream stream)
        {
            int filtersNumber = stream.ReadByte();
            if (filtersNumber < 0)
            {
                throw new EndOfStreamException("Could not read a single byte");
            }

            return (byte) filtersNumber;
        }
    }
}
