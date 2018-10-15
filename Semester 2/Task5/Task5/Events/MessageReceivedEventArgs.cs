using System.Net.Sockets;

namespace Task5.Events
{
    public class MessageReceivedEventArgs
    {
        public string Message { get; }

        public MessageReceivedEventArgs(string message)
        {
            Message = message;
        }
    }
}
