using Core.Utils;
using JetBrains.Annotations;

namespace Core.Logging
{
    public sealed class XmlLogger : ILogger
    {
        [NotNull]
        LockingStringBuilder Builder { get; } = new LockingStringBuilder("<log>");

        public void LogAddStarted() => Builder.Append("<add>");
        public void LogAddFinished() => Builder.Append("</add>");
        public void LogRemoveStarted() => Builder.Append("<remove>");
        public void LogRemoveFinished() => Builder.Append("</remove>");
        public void LogQueryStarted() => Builder.Append("<contains>");
        public void LogQueryFinished() => Builder.Append("</contains>");
        public string Log => Builder + "</log>";
    }
}
