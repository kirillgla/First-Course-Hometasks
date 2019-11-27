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
            Console.WriteLine($"Starting echo server at {ep}...");
            var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            try
            {
                listener.Bind(ep);
                listener.Listen(Backlog);
                Console.WriteLine("Started echo server.");
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
            for (int index = 0; index < host.AddressList.Length; index++)
            {
                var address = host.AddressList[index];
                Console.WriteLine($"{index}: {address}");
            }

            Console.Write($"Please, enter index of the IP address to use (0..{host.AddressList.Length - 1}):");
            int selectedIndex = int.Parse(Console.ReadLine() ?? throw new NullReferenceException());
            return host.AddressList[selectedIndex];
        }

        private static async void HandleClient(Socket client)
        {
            try
            {
                Console.WriteLine("Got a client!");
                var buffer = new byte[BufferSize];
                int received;
                while ((received = await client.ReceiveAsync(buffer, SocketFlags.None)) > 0)
                {
                    Console.Write($"Read {received} bytes from a client:");
                    Console.WriteLine($"{Encoding.ASCII.GetString(buffer, 0, buffer.Length)}");
                    Console.WriteLine($"Sending {received} bytes back");
                    await client.SendAsync(buffer.Take(received).ToArray(), SocketFlags.None);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("A client died!");
                Console.WriteLine(e.Message);
            }
        }
    }
}