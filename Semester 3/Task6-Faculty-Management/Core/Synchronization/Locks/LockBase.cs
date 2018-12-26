using System;

namespace Core.Synchronization.Locks
{
    public abstract class LockBase : ILock
    {
        public abstract void Lock();
        public abstract void Unlock();

        public void RunLocking(Action action) => RunLocking<object>(() =>
        {
            action();
            return null;
        });

        public T RunLocking<T>(Func<T> func)
        {
            try
            {
                Lock();
                return func();
            }
            finally
            {
                Unlock();
            }
        }
    }
}
