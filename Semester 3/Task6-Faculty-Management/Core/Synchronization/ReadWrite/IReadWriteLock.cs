using System;
using JetBrains.Annotations;

namespace Core.Synchronization.ReadWrite
{
    public interface IReadWriteLock
    {
        void AcquireReadLock();
        void ReleaseReadLock();

        void AcquireWriteLock();
        void ReleaseWriteLock();

        void RunReadAction([NotNull] Action action);
        void RunWriteAction([NotNull] Action action);

        T RunReadAction<T>([NotNull] Func<T> function);
        T RunWriteAction<T>([NotNull] Func<T> function);
    }
}
