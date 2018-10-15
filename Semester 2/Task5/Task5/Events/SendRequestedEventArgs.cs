using System;

namespace Task5.Events
{
    public class SendRequestedEventArgs: EventArgs
    {
        public string Message { get; }

        public SendRequestedEventArgs(string message)
        {
            Message = message;
        }
    }
}
