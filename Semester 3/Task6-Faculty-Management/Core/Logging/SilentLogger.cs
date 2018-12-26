namespace Core.Logging
{
    public sealed class SilentLogger : ILogger
    {
        public void LogAddStarted()
        {
        }

        public void LogAddFinished()
        {
        }

        public void LogRemoveStarted()
        {
        }

        public void LogRemoveFinished()
        {
        }

        public void LogQueryStarted()
        {
        }

        public void LogQueryFinished()
        {
        }

        public string Log => "";
    }
}
