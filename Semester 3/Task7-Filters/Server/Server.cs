using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using Tools;
using Tools.Extensions;
using Tools.Functional;
using Tools.Model;

namespace Server
{
    public sealed class Server : IDisposable
    {
        TcpListener Listener { get; }

        IReadOnlyDictionary<string, double[][]> Filters { get; }

        public Server(IPEndPoint endPoint, IReadOnlyDictionary<string, double[][]> filters)
        {
            Listener = new TcpListener(endPoint);
            Filters = filters;
        }

        public void Start()
        {
            Listener.Start();
            HandleRequests();
        }

        async void HandleRequests()
        {
            try
            {
                while (true)
                {
                    var client = await Listener.AcceptTcpClientAsync();
                    HandleRequest(client);
                }
            }
            catch (SocketException)
            {
            }
            catch (Exception e)
            {
                Console.WriteLine($"Got error: {e.Message}");
            }

            Console.WriteLine("Stopping");
        }

        async void HandleRequest(TcpClient client)
        {
            Console.WriteLine("Got connection!");
            try
            {
                var stream = client.GetStream();
                await SendFilters(stream);
                Console.WriteLine("Sent filters!");
                string name = await ReceiveFilterName(stream);
                Console.WriteLine($"Received filter name: {name.Trim()}");
                var kernel = Filters[name];
                int size = await ReceiveSize(stream);
                Console.WriteLine($"Received image size: {size.ReadableSize()}");
                var image = await ReceiveImage(stream, size);
                Console.WriteLine("Received image!");
                var continuation = LightSwitch.Listening(stream);
                var result = image.WithKernelApplied(kernel, OnProgressChanged(stream), continuation);
                Console.WriteLine("Calculated result!");
                if (result is Just<BasicImage> just)
                {
                    await SendImage(stream, just.Value, size);
                    Console.WriteLine("Sent result!");
                }
                else
                {
                    Console.WriteLine("Result is Nothing!");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"Got error while handling request: '{e}'");
            }

            Console.WriteLine("Done!");
        }

        static Action<double> OnProgressChanged(Stream stream) => progress =>
        {
            Console.WriteLine($"Sending progress: {progress}");
            stream.WriteByte(Core.Code.Progress);
            stream.WriteByte((byte) progress);
        };

        async Task SendFilters(Stream stream)
        {
            byte filtersCount = (byte) Filters.Count;
            await stream.WriteAsync(filtersCount);
            foreach (var pair in Filters)
            {
                await stream.WriteAsync(pair.Key);
            }
        }

        static async Task<string> ReceiveFilterName(Stream stream)
        {
            const int size = 256;
            var buffer = new byte[size];
            await stream.ForceReadAsync(buffer, 0, size);
            return Encoding.ASCII.GetString(buffer);
        }

        static async Task<int> ReceiveSize(Stream stream)
        {
            var result = await Int32Representation.Receive(stream);
            return result.ToInt();
        }

        static async Task<BasicImage> ReceiveImage(Stream stream, int size)
        {
            byte[] buffer = new byte[size];
            await stream.ForceReadAsync(buffer, 0, size);
            return new BasicImage(buffer);
        }

        static async Task SendImage(Stream stream, BasicImage image, int size)
        {
            stream.WriteByte(Core.Code.Success);
            await stream.WriteAsync(image.Data, 0, size);
        }

        public void Dispose() => Listener.Stop();
    }
}
