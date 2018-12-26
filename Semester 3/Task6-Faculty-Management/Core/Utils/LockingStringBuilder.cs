using System.Text;
using JetBrains.Annotations;

namespace Core.Utils
{
    public sealed class LockingStringBuilder
    {
        [NotNull]
        object Locker { get; } = new object();

        [NotNull]
        StringBuilder Builder { get; } = new StringBuilder();

        public LockingStringBuilder([CanBeNull] string message = null)
        {
            if (message != null)
            {
                Builder.Append(message);
            }
        }

        public void Append([NotNull] string message)
        {
            lock (Locker)
            {
                Builder.Append(message);
            }
        }

        public override string ToString()
        {
            lock (Locker)
            {
                return Builder.ToString();
            }
        }
    }
}
