using System.IO;
using System.Threading.Tasks;
using Tools.Extensions;

namespace Tools.Model
{
    public struct Int32Representation
    {
        byte B1 { get; } // Most significant digit
        byte B2 { get; }
        byte B3 { get; }
        byte B4 { get; } // Least significant digit

        public Int32Representation(byte b1, byte b2, byte b3, byte b4)
        {
            B1 = b1;
            B2 = b2;
            B3 = b3;
            B4 = b4;
        }

        public static async Task<Int32Representation> Receive(Stream stream)
        {
            const int count = 4;
            var buffer = new byte[count];
            await stream.ForceReadAsync(buffer, 0, count);
            return new Int32Representation(buffer[0], buffer[1], buffer[2], buffer[3]);
        }

        public Task Send(Stream stream)
        {
            const int count = 4;
            var buffer = new[] {B1, B2, B3, B4};
            return stream.WriteAsync(buffer, 0, count);
        }

        public static Int32Representation FromInt(int value)
        {
            const int mask = 0x000000FF;
            byte b1 = (byte) ((value >> 24) & mask);
            byte b2 = (byte) ((value >> 16) & mask);
            byte b3 = (byte) ((value >> 8) & mask);
            byte b4 = (byte) (value & mask);
            return new Int32Representation(b1, b2, b3, b4);
        }

        public int ToInt() => (B1 << 24) + (B2 << 16) + (B3 << 8) + B4;
    }
}
