namespace Core.Synchronization.Locks
{
    public sealed class TasLock : LockBase
    {
        int locked;
        
        public override void Lock()
        {
            while (!Atomic.TestAndSet(ref locked, 1, 0))
            {
            }
        }

        public override void Unlock() => locked = 0;
    }
}
