using System;
using System.Runtime.Serialization;

namespace Task1
{
    /// <summary>
    /// Indicates that current task is aborted.
    /// </summary>
    [Serializable]
    class AbortedException : Exception
    {
        public AbortedException()
        {
        }

        public AbortedException(string message) : base(message)
        {
        }

        public AbortedException(string message, Exception innerException) : base(message, innerException)
        {
        }

        protected AbortedException(SerializationInfo info, StreamingContext context) : base(info, context)
        {
        }
    }
}