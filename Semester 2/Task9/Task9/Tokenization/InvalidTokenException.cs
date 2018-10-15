using System;

namespace Task9.Tokenization
{
    public class InvalidTokenException : Exception
    {
        public InvalidTokenException(string message): base (message)
        {
        }
    }
}
