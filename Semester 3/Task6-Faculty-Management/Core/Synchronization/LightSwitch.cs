using Core.Synchronization.Locks;
using JetBrains.Annotations;

namespace Core.Synchronization
{
    public sealed class LightSwitch
    {
        volatile int counter;

        [NotNull]
        ILock Locker { get; } = new ClrLock();

        public void Lock([NotNull] ILock locker) => Locker.RunLocking(() =>
        {
            counter += 1;
            if (counter == 1)
            {
                locker.Lock();
            }
        });

        public void Unlock([NotNull] ILock locker) => Locker.RunLocking(() =>
        {
            counter -= 1;
            if (counter == 0)
            {
                locker.Unlock();
            }
        });
    }
}
