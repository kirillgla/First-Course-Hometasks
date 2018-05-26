namespace Task5.Events
{
    public class ConnectionsCountChangedEventArgs
    {
        public int Count { get; }

        public ConnectionsCountChangedEventArgs(int count)
        {
            Count = count;
        }
    }
}
