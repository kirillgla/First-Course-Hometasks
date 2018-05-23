namespace Task5.Events
{
    public class ConnectionRequestedEventArgs
    {
        public string Ip { get; }

        public int Port { get; }

        public ConnectionRequestedEventArgs(string ip, int port)
        {
            Port = port;
            Ip = ip;
        }
    }
}
