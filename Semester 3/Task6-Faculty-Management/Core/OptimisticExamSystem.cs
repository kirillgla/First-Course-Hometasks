using System.Collections.Generic;
using Core.Collections;
using Core.Synchronization.Factories;
using JetBrains.Annotations;

namespace Core
{
    public sealed class OptimisticExamSystem : IExamSystem
    {
        [NotNull]
        ISimpleSet<CreditData> Set { get; }

        public OptimisticExamSystem(int hashTableSize, IReadWriteLockFactory lockFactory) =>
            Set = new OptimisticHashSet(hashTableSize, lockFactory);

        public void Add(long studentId, long courseId) => Set.Add(new CreditData(studentId, courseId));
        public void Remove(long studentId, long courseId) => Set.Remove(new CreditData(studentId, courseId));
        public bool Contains(long studentId, long courseId) => Set.Contains(new CreditData(studentId, courseId));

        public IDictionary<long, IList<long>> Contents
        {
            get
            {
                var optimistic = (OptimisticSet) Set;
                IDictionary<long, IList<long>> result = new Dictionary<long, IList<long>>();
                var head = optimistic.GetHeadSoThatToUseItInTests();
                head = head.Next; // skip first fake element
                while (head.Value != CreditData.Max)
                {
                    var data = head.Value;
                    if (result.ContainsKey(data.StudentId))
                    {
                        var courses = result[data.StudentId];
                        courses.Add(data.CourseId);
                    }
                    else
                    {
                        var courses = new List<long> {data.CourseId};
                        result[data.StudentId] = courses;
                    }

                    head = head.Next;
                }

                return result;
            }
        }
    }
}
