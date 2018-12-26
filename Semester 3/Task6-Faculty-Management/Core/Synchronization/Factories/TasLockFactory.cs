using Core.Synchronization.Locks;

namespace Core.Synchronization.Factories
{
    public sealed class TasLockFactory : ILockFactory
    {
        public ILock Create() => new TasLock();
    }
}
