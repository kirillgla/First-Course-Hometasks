using JetBrains.Annotations;

namespace Core.Logging
{
    public interface ILogger
    {
        void LogAddStarted();
        void LogAddFinished();
        void LogRemoveStarted();
        void LogRemoveFinished();
        void LogQueryStarted();
        void LogQueryFinished();

        [NotNull]
        string Log { get; }
    }
}
