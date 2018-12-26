using System.Collections.Generic;
using Core.Utils;
using JetBrains.Annotations;

namespace Core
{
    public interface IExamSystem
    {
        void Add(long studentId, long courseId);
        void Remove(long studentId, long courseId);
        bool Contains(long studentId, long courseId);

        [NotNull]
        [TestOnly]
        // This should only be called
        // when all threads accessing the collection are dead
        IDictionary<long, IList<long>> Contents { get; }
    }
}
