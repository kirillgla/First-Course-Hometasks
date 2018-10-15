using System;
using System.Net.Sockets;
using System.Threading.Tasks;
using Task5.Events;

namespace Task5
{
    public abstract class AbstractClient : IClientInformation, IClientEvents
    {
        public abstract event EventHandler<MessageReceivedEventArgs> MessageReceived;
        public abstract event EventHandler<ConnectionsCountChangedEventArgs> ConnectionsCountChanged;
        public abstract event EventHandler ListeningStateChanged;


        public abstract int IncomingConnectionsCount { get; }
        public abstract string OutcomingConnectionIp { get; }
        public abstract int ListeningPort { get; }
        public abstract bool HasConnections { get; }

        public bool IsListening => ListeningPort != -1;
        public bool HasOutcomingConnection => OutcomingConnectionIp != null;
        public int ConnectionsCount => IncomingConnectionsCount + (HasOutcomingConnection? 1: 0);
        
        /// <summary>
        /// Begins accepting incoming connections
        /// </summary>
        public abstract void StartListening(int port);

        /// <summary>
        /// Stops accepting incoming connections
        /// </summary>
        public abstract void StopListening();

        public abstract void TerminateIncomingConnections();

        public abstract Task Connect(string ip, int port);

        public abstract void Disconnect();

        /// <summary>
        /// Sends message to all connected devices
        /// </summary>
        /// <param name="message">Text to be sent</param>
        public abstract Task Send(string message);

        public abstract Task Send(MessageData data);
        
        /// <summary>
        /// Sends message to all connected devices but for one
        /// </summary>
        /// <param name="message">Text to be sent</param>
        /// <param name="ignore">Connection with device to be ignored</param>
        public abstract Task Send(string message, Socket ignore);

        public abstract void SendDisconnectNotifications();
    }
}
