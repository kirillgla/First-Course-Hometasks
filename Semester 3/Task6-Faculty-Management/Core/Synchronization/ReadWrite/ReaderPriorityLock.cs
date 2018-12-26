using System.Threading;
using Core.Synchronization.Locks;
using JetBrains.Annotations;

namespace Core.Synchronization.ReadWrite
{
    public sealed class ReaderPriorityLock : ReadWriteLockBase
    {
        [NotNull]
        ILock ReaderLock { get; }

        // Should not care about which thread releases it
        [NotNull]
        ILock GlobalLock { get; }

        volatile int readersCount;

        public ReaderPriorityLock()
        {
            ReaderLock = new ClrLock();
            GlobalLock = new TtasLock();
        }

        public override void AcquireReadLock() => ReaderLock.RunLocking(() =>
        {
            int incremented = Interlocked.Increment(ref readersCount);
            if (incremented == 1)
            {
                GlobalLock.Lock();
            }
        });

        public override void ReleaseReadLock() => ReaderLock.RunLocking(() =>
        {
            int decremented = Interlocked.Decrement(ref readersCount);
            if (decremented == 0)
            {
                GlobalLock.Unlock();
            }
        });

        public override void AcquireWriteLock() => GlobalLock.Lock();

        public override void ReleaseWriteLock() => GlobalLock.Unlock();
    }
}
