using JetBrains.Annotations;

namespace Core.Synchronization.ReadWrite
{
    public static class ReadWriteUtils
    {
        public static void ReadLocker([NotNull] IReadWriteLock locker) => locker.AcquireReadLock();
        public static void WriteLocker([NotNull] IReadWriteLock locker) => locker.AcquireWriteLock();
        public static void ReadUnlocker([NotNull] IReadWriteLock locker) => locker.ReleaseReadLock();
        public static void WriteUnlocker([NotNull] IReadWriteLock locker) => locker.ReleaseWriteLock();
    }
}
