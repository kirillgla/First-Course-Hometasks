using System.Collections.Generic;
using Core.Synchronization.Factories;
using Core.Synchronization.Locks;
using Core.Synchronization.ReadWrite;
using JetBrains.Annotations;

namespace Core.Utils
{
    /// <summary>
    /// Stores data associated with one student
    /// </summary>
    public sealed class StudentDataSubstorage<T>
    {
        public StudentDataSubstorage([NotNull] IReadWriteLock locker, [NotNull] IList<T> data)
        {
            Locker = locker;
            Data = data;
        }

        [NotNull]
        public IReadWriteLock Locker { get; }

        [NotNull]
        public IList<T> Data { get; }
    }
}
