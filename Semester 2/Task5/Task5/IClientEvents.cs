using System;
using Task5.Events;

namespace Task5
{
    public interface IClientEvents
    {
        event EventHandler<MessageReceivedEventArgs> MessageReceived;
        event EventHandler<ConnectionsCountChangedEventArgs> ConnectionsCountChanged;
        event EventHandler ListeningStateChanged;
    }
}
