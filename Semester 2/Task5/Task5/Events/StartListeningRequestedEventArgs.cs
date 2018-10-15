namespace Task5.Events
{
    public class StartListeningRequestedEventArgs
    {
        public int ListeningPort { get; }

        public StartListeningRequestedEventArgs(int listeningPort)
        {
            ListeningPort = listeningPort;
        }
    }
}
