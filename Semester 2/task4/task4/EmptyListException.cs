using System;

namespace Task4
{
    /// <summary>
    /// This exception is thrown
    /// when attempting to retrive an element
    /// from empty list
    /// </summary>
    class EmptyListException : Exception
    {
        public EmptyListException() : base() { }
        public EmptyListException(string message) : base(message) { }
        public EmptyListException(string message, Exception innerException) : base(message, innerException) { }
        public EmptyListException(System.Runtime.Serialization.SerializationInfo info,
                                  System.Runtime.Serialization.StreamingContext context) : base(info, context) { }
    }
}
