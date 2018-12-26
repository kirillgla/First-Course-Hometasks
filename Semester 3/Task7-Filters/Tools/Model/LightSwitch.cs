using System;
using System.IO;

namespace Tools.Model
{
    public sealed class LightSwitch
    {
        public bool IsActive { get; private set; }
        public void Shut() => IsActive = false;
        public LightSwitch() => IsActive = true;

        public async void Listen(Stream stream)
        {
            byte[] buffer = new byte[1];
            try
            {
                await stream.ReadAsync(buffer, 0, 1);
            }
            catch (Exception)
            {
                // Ignore
            }

            Shut();
        }

        public static LightSwitch Listening(Stream stream)
        {
            var result = new LightSwitch();
            result.Listen(stream);
            return result;
        }
    }
}
