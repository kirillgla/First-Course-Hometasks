using System;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using Common;

namespace EchoServer
{
    internal static class Program
    {
        private const int Backlog = 100;

        private const int BufferSize = 1024;

        private static async Task Main()
        {
            var ep = new IPEndPoint(GetHostIp(), Constants.Port);
            Console.WriteLine($"Starting echo server at {ep}");
            var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            try
            {
                listener.Bind(ep);
                listener.Listen(Backlog);
                while (true)
                {
                    var client = await listener.AcceptAsync();
                    HandleClient(client);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
                Console.WriteLine("Error occurred. Echo server shut down.");
            }
        }

        private static IPAddress GetHostIp()
        {
            string hostName = Dns.GetHostName();
            Console.WriteLine($"Host name: {hostName}");
            var host = Dns.GetHostEntry(hostName);
            Console.WriteLine("Host addresses:");
            foreach (var address in host.AddressList)
            {
                Console.WriteLine($" - {address}");
            }

            return host.AddressList[0];
        }

        private static async void HandleClient(Socket client)
        {
            var buffer = new byte[BufferSize];
            int received;
            while ((received = await client.ReceiveAsync(buffer, SocketFlags.None)) > 0)
            {
                Console.WriteLine($"Read {received} bytes from a client:");
                Console.WriteLine($"{Encoding.ASCII.GetString(buffer, 0, buffer.Length)}");
                await client.SendAsync(buffer.Take(received).ToArray(), SocketFlags.None);
            }
        }
    }
}