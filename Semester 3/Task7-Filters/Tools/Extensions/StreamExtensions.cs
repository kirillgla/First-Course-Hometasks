using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace Tools.Extensions
{
    // Oh, I bet those guys in Microsoft could've let us send arbitrary types over network 
    public static class StreamExtensions
    {
        public static Task WriteAsync(this Stream stream, byte[] message) =>
            stream.WriteAsync(message, 0, message.Length);

        public static Task WriteAsync(this Stream stream, byte message) =>
            stream.WriteAsync(new[] {message});

        public static Task WriteAsync(this Stream stream, string message) =>
            stream.WriteAsync(Encoding.ASCII.GetBytes(message));

        // This is only accessed from one thread
        static byte[] TinyBuffer { get; } = new byte[1];

        public static async Task<byte> ForceReadByteAsync(this Stream stream)
        {
            int bytesRead = await stream.ReadAsync(TinyBuffer, 0, 1);
            if (bytesRead != 1)
            {
                throw new EndOfStreamException("Could not read a single byte");
            }

            return TinyBuffer[0];
        }

        public static async Task ForceReadAsync(
            this Stream stream,
            byte[] buffer,
            int start,
            int size
        )
        {
            int bytesRead = await stream.ReadAsync(buffer, start, size);
            if (bytesRead != size)
            {
                int newSize = size - bytesRead;
                // This might potentially lead to infinite recursion
                // if remote device acts unexpectedly, but I trust
                // booth my server and my client
                await stream.ForceReadAsync(buffer, start + bytesRead, newSize);
            }
        }
    }
}
