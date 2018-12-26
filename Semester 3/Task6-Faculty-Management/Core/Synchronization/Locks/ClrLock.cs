using System.Threading;
using JetBrains.Annotations;

namespace Core.Synchronization.Locks
{
    public sealed class ClrLock : LockBase
    {
        [NotNull]
        object Locker { get; } = new object();

        public override void Lock() => Monitor.Enter(Locker);
        public override void Unlock() => Monitor.Exit(Locker);
    }
}
