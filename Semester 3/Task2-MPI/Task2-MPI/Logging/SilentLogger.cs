namespace Task2_MPI.Logging
{
    public sealed class SilentLogger : ILogger
    {
        public void Log(string message)
        {
        }

        public void Assert(bool condition, string message)
        {
        }
    }
}
