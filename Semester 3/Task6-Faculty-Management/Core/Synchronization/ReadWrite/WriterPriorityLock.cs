using Core.Synchronization.Locks;
using JetBrains.Annotations;

namespace Core.Synchronization.ReadWrite
{
    /// <summary>
    /// Based on Little Book Of Semaphores, 4.2.5
    /// </summary>
    public sealed class WriterPriorityLock : ReadWriteLockBase
    {
        [NotNull]
        LightSwitch ReadSwitch { get; }

        // Should support releasing from a different thread
        [NotNull]
        ILock GlobalLock { get; }

        [NotNull]
        ILock Turnstile { get; }

        public WriterPriorityLock()
        {
            ReadSwitch = new LightSwitch();
            GlobalLock = new TtasLock();
            Turnstile = new ClrLock();
        }

        public override void AcquireReadLock()
        {
            Turnstile.RunLocking(() => { });
            ReadSwitch.Lock(GlobalLock);
        }

        public override void ReleaseReadLock()
        {
            ReadSwitch.Unlock(GlobalLock);
        }

        public override void AcquireWriteLock()
        {
            Turnstile.Lock();
            GlobalLock.Lock();
        }

        public override void ReleaseWriteLock()
        {
            GlobalLock.Unlock();
            Turnstile.Unlock();
        }
    }
}
