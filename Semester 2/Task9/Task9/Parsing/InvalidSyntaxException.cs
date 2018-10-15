using System;

namespace Task9.Parsing
{
    public class InvalidSyntaxException : Exception
    {
        public InvalidSyntaxException()
        {
        }

        public InvalidSyntaxException(string message) : base(message)
        {
        }
    }
}
