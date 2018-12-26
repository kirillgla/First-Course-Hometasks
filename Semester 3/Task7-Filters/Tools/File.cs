using System.IO;
using System.Threading.Tasks;
using Tools.Extensions;

namespace Tools
{
    public static class File
    {
        public static async Task<byte[]> ReadAllBytesAsync(string path)
        {
            using (var stream = System.IO.File.OpenRead(path))
            {
                var result = new byte[stream.Length];
                await stream.ReadAsync(result, 0, (int) stream.Length);
                return result;
            }
        }

        public static async Task WriteAllBytesAsync(string path, byte[] data)
        {
            using (var stream = System.IO.File.OpenWrite(path))
            {
                await stream.WriteAsync(data);
            }
        }
    }
}
