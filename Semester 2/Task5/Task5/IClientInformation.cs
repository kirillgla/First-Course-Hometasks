namespace Task5
{
    public interface IClientInformation
    {
        int ListeningPort { get; }
        int IncomingConnectionsCount { get; }
        string OutcomingConnectionIp { get; }
        bool HasOutcomingConnection { get; }
        bool IsListening { get; }
        int ConnectionsCount { get; }
        bool HasConnections { get; }
    }
}
