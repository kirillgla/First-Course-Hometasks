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
            var ip = IPAddress.Parse(server);
            var ep = new IPEndPoint(ip, port);
            var socket = new Socket(ep.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
            socket.Connect(ep);
            return socket;
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
