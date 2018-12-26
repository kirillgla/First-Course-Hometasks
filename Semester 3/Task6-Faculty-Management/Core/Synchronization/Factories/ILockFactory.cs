using Core.Synchronization.Locks;
using JetBrains.Annotations;

namespace Core.Synchronization.Factories
{
    public interface ILockFactory
    {
        [NotNull]
        ILock Create();
    }
}
