using System;
using System.Collections;
using System.Collections.Generic;
using Core.Synchronization.Factories;
using Core.Synchronization.Locks;
using JetBrains.Annotations;

namespace Core.Utils
{
    public sealed class LateinitConcurrentStudentDataStorage : StudentDataStorageBase<long>
    {
        public LateinitConcurrentStudentDataStorage([NotNull] ILock globalLock) => GlobalLock = globalLock;

        public LateinitConcurrentStudentDataStorage([NotNull] ILockFactory factory) : this(factory.Create())
        {
        }

        [NotNull]
        ILock GlobalLock { get; }

        [NotNull]
        IDictionary<long, StudentDataSubstorage<long>> Storage { get; } =
            new Dictionary<long, StudentDataSubstorage<long>>();

        public override U ReadSubstorage<U>(long studentId, Func<IReadOnlyList<long>, U> func)
        {
            GlobalLock.RunLocking(() =>
            {
                StudentDataSubstorage<long> substorage;
                if (Storage.ContainsKey(studentId))
                {
                }
            });
        }

        public override U WriteSubstorage<U>(long studentId, Func<IList<long>, U> func) => throw new NotImplementedException();
    }
}
