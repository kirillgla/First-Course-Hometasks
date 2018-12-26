using System;
using System.Collections.Generic;

namespace Core.Utils
{
    public abstract class StudentDataStorageBase<T> : IStudentDataStorage<T>
    {
        public void ReadSubstorage(long studentId, Action<IReadOnlyList<T>> action) => ReadSubstorage<object>(studentId,
            list =>
            {
                action(list);
                return null;
            }
        );

        public void WriteSubstorage(long studentId, Action<IList<T>> action) => WriteSubstorage<object>(studentId,
            list =>
            {
                action(list);
                return null;
            }
        );

        public abstract U ReadSubstorage<U>(long studentId, Func<IReadOnlyList<T>, U> func);
        public abstract U WriteSubstorage<U>(long studentId, Func<IList<T>, U> func);
    }
}
