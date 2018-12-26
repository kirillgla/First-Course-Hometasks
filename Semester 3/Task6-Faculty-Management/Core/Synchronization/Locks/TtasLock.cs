namespace Core.Synchronization.Locks
{
    public sealed class TtasLock : LockBase
    {
        int locked;
        
        public override void Lock()
        {
            while (true)
            {
                while (locked == 1)
                {
                }

                if (Atomic.TestAndSet(ref locked, 1, 0))
                {
                    break;
                }
            }
        }

        public override void Unlock()
        {
            locked = 0;
        }
    }
}
