using Core.Synchronization.ReadWrite;
using JetBrains.Annotations;

namespace Core.Synchronization.Factories
{
    public sealed class ReadPriorityLockFactory : IReadWriteLockFactory
    {
        public IReadWriteLock Create() => new ReaderPriorityLock();
    }
}
