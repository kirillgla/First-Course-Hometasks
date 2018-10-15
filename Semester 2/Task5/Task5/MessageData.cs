using System.Net.Sockets;

namespace Task5
{
    public sealed class MessageData
    {
        public string Message { get; }
        
        public Socket Socket { get; }

        public MessageData(string message, Socket socket)
        {
            Message = message;
            Socket = socket;
        }
    }
}
