using System;

namespace Task2_MPI.Logging
{
    public sealed class ConsoleLogger : ILogger
    {
        public void Log(string message) => Console.WriteLine(message);

        public void Assert(bool condition, string message)
        {
            if (!condition)
            {
                throw new InvalidOperationException(message);
            }
        }
    }
}
