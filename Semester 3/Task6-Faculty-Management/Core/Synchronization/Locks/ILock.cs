using System;

namespace Core.Synchronization.Locks
{
    public interface ILock
    {
        void Lock();
        void Unlock();
        void RunLocking(Action action);
        T RunLocking<T>(Func<T> func);
    }
}
