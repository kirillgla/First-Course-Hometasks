using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using Common;

namespace EchoClient
{
    public static class Program
    {
        private const int BufferSize = 1024;

        private static Socket ConnectSocket(string server, int port)
        {
            var hostEntry = Dns.GetHostEntry(server);

            // Loop through the AddressList to obtain the supported AddressFamily. This is to avoid
            // an exception that occurs when the host IP Address is not compatible with the address family
            // (typical in the IPv6 case).
            foreach (var address in hostEntry.AddressList)
            {
                var ep = new IPEndPoint(address, port);
                var socket = new Socket(ep.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
                socket.Connect(ep);
                if (!socket.Connected) continue;
                return socket;
            }

            return null;
        }

        private static void DoOnePing(Socket socket)
        {
            var bytesToSend = Encoding.ASCII.GetBytes("HeLlO, wOrLd!?");
            socket.Send(bytesToSend, bytesToSend.Length, 0);
            Console.WriteLine("Sent a request");
            var buffer = new byte[BufferSize];
            int received = socket.Receive(buffer, buffer.Length, 0);
            Console.WriteLine($"Received a response: {Encoding.ASCII.GetString(buffer, 0, received)}");

        }

        public static void Main()
        {
            Console.Write("Please, enter IP to ping:");
            string ip = Console.ReadLine();
            try
            {
                using var socket = ConnectSocket(ip, Constants.Port);
                if (socket == null)
                {
                    Console.WriteLine("Connection failed");
                    return;
                }

                while (true)
                {
                    DoOnePing(socket);
                    Thread.Sleep(1000);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
                Console.WriteLine("Error occurred. Ping stopped.");
            }
        }
    }
}