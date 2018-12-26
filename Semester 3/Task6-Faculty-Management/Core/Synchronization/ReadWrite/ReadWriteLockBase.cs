using System;

namespace Core.Synchronization.ReadWrite
{
    public abstract class ReadWriteLockBase : IReadWriteLock
    {
        public abstract void AcquireReadLock();
        public abstract void ReleaseReadLock();
        public abstract void AcquireWriteLock();
        public abstract void ReleaseWriteLock();

        public void RunReadAction(Action action)
        {
            try
            {
                AcquireReadLock();
                action();
            }
            finally
            {
                ReleaseReadLock();
            }
        }

        public void RunWriteAction(Action action)
        {
            try
            {
                AcquireWriteLock();
                action();
            }
            finally
            {
                ReleaseWriteLock();
            }
        }

        public T RunReadAction<T>(Func<T> function)
        {
            try
            {
                AcquireReadLock();
                return function();
            }
            finally
            {
                ReleaseReadLock();
            }
        }

        public T RunWriteAction<T>(Func<T> function)
        {
            try
            {
                AcquireWriteLock();
                return function();
            }
            finally
            {
                ReleaseWriteLock();
            }
        }
    }
}
