using Core.Synchronization.Locks;

namespace Core.Synchronization.Factories
{
    public sealed class ClrLockFactory : ILockFactory
    {
        public ILock Create() => new ClrLock();
    }
}
