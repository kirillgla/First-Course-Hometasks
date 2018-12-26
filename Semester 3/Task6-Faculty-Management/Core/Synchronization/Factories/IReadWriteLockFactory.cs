using Core.Synchronization.ReadWrite;
using JetBrains.Annotations;

namespace Core.Synchronization.Factories
{
    public interface IReadWriteLockFactory
    {
        [NotNull]
        IReadWriteLock Create();
    }
}
