using System;
using System.IO;
using System.Threading.Tasks;
using Tools;
using Tools.Extensions;
using Tools.Model;

namespace Client
{
    public abstract class RequestBase
    {
        string Filter { get; }

        protected RequestBase(string filter) => Filter = filter.Extend();

        public async Task Handle(Stream stream, Action<byte> onProgressChanged)
        {
            await stream.WriteAsync(Filter);
            var buffer = await SendImage(stream);
            await TrackProgress(stream, onProgressChanged);
            await stream.ForceReadAsync(buffer, 0, buffer.Length);
            await SaveResult(buffer);
        }

        static async Task TrackProgress(Stream stream, Action<byte> onProgressChanged)
        {
            while (true)
            {
                byte status = await stream.ForceReadByteAsync();
                if (status != Core.Code.Progress)
                {
                    return;
                }

                byte progress = await stream.ForceReadByteAsync();
                onProgressChanged(progress);
            }
        }

        async Task<byte[]> SendImage(Stream stream)
        {
            byte[] bytes = await GetSource();
            await Int32Representation.FromInt(bytes.Length).Send(stream);
            await stream.WriteAsync(bytes);
            return bytes;
        }

        protected abstract Task<byte[]> GetSource();

        protected abstract Task SaveResult(byte[] result);
    }
}
