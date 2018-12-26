using System.Collections.Generic;
using System.Collections.ObjectModel;
using Core.Logging;
using Core.Synchronization.ReadWrite;
using JetBrains.Annotations;

namespace Core
{
    public sealed class GlobalLockExamSystem : IExamSystem
    {
        public GlobalLockExamSystem([NotNull] IReadWriteLock locker, [NotNull] ILogger logger)
        {
            Locker = locker;
            Logger = logger;
        }

        [NotNull]
        IReadWriteLock Locker { get; }

        [NotNull]
        IDictionary<long, IList<long>> Accepted { get; } = new Dictionary<long, IList<long>>();

        [NotNull]
        ILogger Logger { get; }

        public IDictionary<long, IList<long>> Contents =>
            new ReadOnlyDictionary<long, IList<long>>(Accepted);

        public void Add(long studentId, long courseId) => Locker.RunWriteAction(() =>
        {
            Logger.LogAddStarted();
            if (Accepted.ContainsKey(studentId))
            {
                Accepted[studentId]?.Add(courseId);
                Logger.LogAddFinished();
                return;
            }

            Accepted[studentId] = new List<long> {courseId};
            Logger.LogAddFinished();
        });

        public void Remove(long studentId, long courseId) => Locker.RunWriteAction(() =>
        {
            Logger.LogRemoveStarted();
            if (!Accepted.ContainsKey(studentId))
            {
                Logger.LogRemoveFinished();
                return;
            }

            Accepted[studentId]?.Remove(courseId);
            Logger.LogRemoveFinished();
        });

        public bool Contains(long studentId, long courseId) => Locker.RunReadAction(() =>
        {
            Logger.LogQueryStarted();
            if (!Accepted.ContainsKey(studentId))
            {
                Logger.LogQueryFinished();
                return false;
            }

            bool result = Accepted[studentId]?.Contains(courseId) ?? false;
            Logger.LogQueryFinished();
            return result;
        });
    }
}
