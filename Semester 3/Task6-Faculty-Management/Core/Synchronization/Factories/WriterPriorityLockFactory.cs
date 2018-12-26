using Core.Synchronization.ReadWrite;

namespace Core.Synchronization.Factories
{
    public sealed class WriterPriorityLockFactory : IReadWriteLockFactory
    {
        public IReadWriteLock Create() => new WriterPriorityLock();
    }
}
