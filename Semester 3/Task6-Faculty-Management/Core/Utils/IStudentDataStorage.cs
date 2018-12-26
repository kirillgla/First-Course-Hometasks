using System;
using System.Collections.Generic;
using JetBrains.Annotations;

namespace Core.Utils
{
    public interface IStudentDataStorage<T>
    {
        void ReadSubstorage(long studentId, [NotNull] Action<IReadOnlyList<T>> action);
        void WriteSubstorage(long studentId, [NotNull] Action<IList<T>> action);

        U ReadSubstorage<U>(long studentId, [NotNull] Func<IReadOnlyList<T>, U> func);
        U WriteSubstorage<U>(long studentId, [NotNull] Func<IList<T>, U> func);
    }
}
