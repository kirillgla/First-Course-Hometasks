using System.Threading;

namespace Core.Synchronization.Locks
{
    public sealed class ThreadIgnorantClrLock : LockBase
    {
        const int HotWaitLimit = 100;

        const int Unlocked = 0;
        const int Locked = 1;

        int flag = Unlocked;

        public override void Lock()
        {
            while (true)
            {
                WaitUnlocked();
                if (Atomic.TestAndSet(ref flag, Locked, Unlocked))
                {
                    break;
                }
            }
        }

        void WaitUnlocked()
        {
            while (flag == Locked)
            {
                for (int index = 0; index < HotWaitLimit; index += 1)
                {
                    if (flag != Locked)
                    {
                        return;
                    }
                }

                Thread.Sleep(0);
            }
        }

        public override void Unlock()
        {
            flag = 0;
        }
    }
}
