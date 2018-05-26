using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Windows;
using Task5.Events;

namespace Task5
{
    public sealed class Client : AbstractClient
    {
        const int BufferSize = 1024;

        public Client()
        {
            incomingConnections = new List<Socket>();
        }

        public override event EventHandler<MessageReceivedEventArgs> MessageReceived;
        public override event EventHandler<ConnectionsCountChangedEventArgs> ConnectionsCountChanged;
        public override event EventHandler ListeningStateChanged;

        #region private members

        Socket listeningSocet;
        Socket outcomingConnection;
        readonly IList<Socket> incomingConnections;

        string IpAddressMessage => outcomingConnection?.LocalEndPoint?.ToString();

        // ==== disconnection management ====

        Socket GetAnyConnectedDevice()
        {
            if (!HasConnections)
            {
                throw new InvalidOperationException("No connections");
            }

            // Doesn't throw
            return HasOutcomingConnection ? outcomingConnection : incomingConnections[0];
        }

        const string DisconnectingMessageStart = "I am disconnecting and politely ask you to connect to ";
        const string DisconnectingMessagePattern = DisconnectingMessageStart + @"'(.{1,})':'(\d{1,})'";

        [Pure]
        public static bool IsDisconnectingMessage(string message)
        {
            return Regex.IsMatch(message, DisconnectingMessagePattern);
        }

        [Pure]
        public static IPEndPoint ParseIpEndPoint(string message)
        {
            var match = Regex.Match(message, DisconnectingMessagePattern);

            if (match.Success)
            {
                var address = IPAddress.Parse(match.Groups[1].Value);
                int port = int.Parse(match.Groups[2].Value);
                
                var endPoint = new IPEndPoint(address, port);

                return endPoint;
            }
            
            throw new ArgumentException(nameof(message));
        }

        [Pure]
        public static string MakeDisconnectingNotification(Socket target)
        {
            return DisconnectingMessageStart + '\'' + (target.RemoteEndPoint as IPEndPoint)?.Address + "':'" +
                   (target.RemoteEndPoint as IPEndPoint)?.Port + '\'';
        }

        public override void SendDisconnectNotifications()
        {
            if (!HasConnections)
            {
                return;
            }

            var target = GetAnyConnectedDevice();
            string message = MakeDisconnectingNotification(target);

            Send(message, target).Ignore();
        }

        // ==== ====

        async void StartReceivingMessages(Socket socket)
        {
            byte[] array = new byte[BufferSize];
            var segment = new ArraySegment<byte>(array);

            while (true)
            {
                if (!socket.Connected)
                {
                    StopHandling(socket);
                    return;
                }

                int bytesRead;
                try
                {
                    bytesRead = await socket.ReceiveAsync(segment, SocketFlags.None);
                }
                catch (Exception)
                {
                    StopHandling(socket);
                    return;
                }

                string message = Encoding.Unicode.GetString(array, 0, bytesRead);

                if (string.IsNullOrWhiteSpace(message))
                {
                    StopHandling(socket);
                    MessageBox.Show("One of devices must have been disconnected", "Error: empty string received");
                    return;
                }

                if (message == IpAddressMessage)
                {
                    Disconnect();
                    MessageBox.Show("Disconnected due to loops in network", "Connection terminated");
                    ConnectionsCountChanged?.Invoke(this, new ConnectionsCountChangedEventArgs(ConnectionsCount));
                    return;
                }

                if (IsDisconnectingMessage(message))
                {
                    Disconnect();
                    var remoteEndPoint = ParseIpEndPoint(message);
                    await Connect(remoteEndPoint);
                }

                MessageReceived?.Invoke(this, new MessageReceivedEventArgs(message));
                await Send(message, socket);
            }
        }

        void StopHandling(Socket socket)
        {
            if (socket == outcomingConnection)
            {
                Disconnect();
                return;
            }

            if (incomingConnections.Remove(socket))
            {
                ConnectionsCountChanged?.Invoke(this, new ConnectionsCountChangedEventArgs(ConnectionsCount));
                return;
            }

            // throw new InvalidOperationException("StopHandling() was given a socket that doesn't belong to client");
            // MessageBox.Show("StopHandling() was given a socket that doesn't belong to client", "Warning");
        }

        #endregion private members

        public override int IncomingConnectionsCount => incomingConnections.Count;

        public override int ListeningPort => (listeningSocet?.LocalEndPoint as IPEndPoint)?.Port ?? -1;

        public override string OutcomingConnectionIp =>
            (outcomingConnection?.LocalEndPoint as IPEndPoint)?.Address?.ToString();

        public override bool HasConnections => HasOutcomingConnection || IncomingConnectionsCount > 0;

        #region public methods

        public override void StartListening(int port)
        {
            if (IsListening)
            {
                throw new InvalidOperationException("Should stop listening before restarting.");
            }

            // Establish the local endpoint for the socket.    
            var ipHostInfo = Dns.GetHostEntry(Dns.GetHostName());
            var ipAddress = ipHostInfo.AddressList[0];
            var localEndPoint = new IPEndPoint(ipAddress, port);

            // Create a TCP/IP socket.  
            var listener = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

            // Bind the socket to the local endpoint
            listener.Bind(localEndPoint);

            listener.Listen(100);

            // I'm not sure about thread-safety of this assignment.
            // Let's just hope code above executes quickly enough to prevent problems
            listeningSocet = listener;

            ListeningStateChanged?.Invoke(this, EventArgs.Empty);

            Handle();
        }

        async void Handle()
        {
            // Listen for connections
            while (true)
            {
                if (!IsListening)
                {
                    return;
                }

                Socket handler;
                try
                {
                    // this operation is canceled when socket gets disposed
                    handler = await listeningSocet.AcceptAsync();
                    StartReceivingMessages(handler);
                }
                catch (SocketException)
                {
                    return;
                }

                incomingConnections.Add(handler);
                ConnectionsCountChanged?.Invoke(this, new ConnectionsCountChangedEventArgs(ConnectionsCount));
            }
        }

        public override void StopListening()
        {
            if (!IsListening)
            {
                throw new InvalidOperationException("Should start listening before stopping.");
            }

            try
            {
                listeningSocet.Close();
            }
            catch (Exception e)
            {
                MessageBox.Show(e.ToString(), "Error");
            }

            listeningSocet = null;
            ListeningStateChanged?.Invoke(this, EventArgs.Empty);
        }

        public override void TerminateIncomingConnections()
        {
            // do I really need to lock?
            // Probably not, but I'm afraid to modify this old code
            lock (incomingConnections)
            {
                foreach (var socket in incomingConnections)
                {
                    socket.Shutdown(SocketShutdown.Both);
                    socket.Close();
                }

                incomingConnections.Clear();
            }

            ConnectionsCountChanged?.Invoke(this, new ConnectionsCountChangedEventArgs(ConnectionsCount));
        }

        public override async Task Connect(string ip, int port)
        {
            if (HasOutcomingConnection)
            {
                throw new InvalidOperationException("Should disconnect before estabinishing another connection.");
            }

            // Establish the remote endpoint for the socket.    
            var ipHostInfo = Dns.GetHostEntry(ip);
            var ipAddress = ipHostInfo.AddressList[0];
            var remoteEndPoint = new IPEndPoint(ipAddress, port);

            var localHostInfo = Dns.GetHostEntry(Dns.GetHostName());

            if (IsListening && ListeningPort == port &&
                Equals(ipHostInfo.AddressList[0], localHostInfo.AddressList[0]))
            {
                throw new InvalidOperationException("Cannot connect to itself");
            }

            await Connect(remoteEndPoint);
        }

        async Task Connect(EndPoint remoteEndPoint)
        {
            // Create a TCP/IP socket.
            var client = new Socket(remoteEndPoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

            // Connect to the remote endpoint. 
            await client.ConnectAsync(remoteEndPoint);

            outcomingConnection = client;

            StartReceivingMessages(client);

            // This action should terminate current connection if loops are present
            await Send(IpAddressMessage);

            ConnectionsCountChanged?.Invoke(this, new ConnectionsCountChangedEventArgs(ConnectionsCount));
        }

        public override void Disconnect()
        {
            bool actuallyDisconnecting = HasOutcomingConnection;

            outcomingConnection?.Shutdown(SocketShutdown.Both);
            outcomingConnection?.Close();
            outcomingConnection = null;

            if (actuallyDisconnecting)
            {
                ConnectionsCountChanged?.Invoke(this, new ConnectionsCountChangedEventArgs(ConnectionsCount));
            }
        }

        public override Task Send(string message)
        {
            return Send(message, null);
        }

        public override Task Send(MessageData data)
        {
            return Send(data.Message, data.Socket);
        }

        public override async Task Send(string message, Socket ignore)
        {
            if (string.IsNullOrWhiteSpace(message))
            {
                throw new ArgumentException(nameof(message));
            }

            if (!HasConnections)
            {
                throw new InvalidOperationException("Should get connections before sending messages.");
            }

            var data = new ArraySegment<byte>(Encoding.Unicode.GetBytes(message));

            if (HasOutcomingConnection && outcomingConnection != ignore)
            {
                await outcomingConnection.SendAsync(data, SocketFlags.None);
            }

            foreach (var socket in incomingConnections)
            {
                if (socket != ignore)
                {
                    await socket.SendAsync(data, 0);
                }
            }
        }

        #endregion public methods
    }
}
