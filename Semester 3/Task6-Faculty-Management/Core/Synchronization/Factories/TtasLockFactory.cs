using Core.Synchronization.Locks;

namespace Core.Synchronization.Factories
{
    public sealed class TtasLockFactory : ILockFactory
    {
        public ILock Create() => new TtasLock();
    }
}
