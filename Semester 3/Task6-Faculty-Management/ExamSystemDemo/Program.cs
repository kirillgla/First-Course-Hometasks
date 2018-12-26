using System;
using System.Resources;
using System.Threading.Tasks;
using Core;
using Core.Synchronization.Factories;
using JetBrains.Annotations;
using static ExamSystemDemo.ConsoleUtils;

namespace ExamSystemDemo
{
    static class Program
    {
        // (Total number of worker threads) / TaskScaleFactor
        public const int TaskNumberScale = 20;

        // Max number of requests per worker
        public const int TaskSize = 50;

        // This magic constant is 100 because minimum scaling
        // involves 90 readers + 9 adders + 1 remover
        public const int TaskScaleFactor = 100;
        
        public const int HashTableSize = 10000;

        static async Task Main()
        {
            Console.WriteLine($"Running {TaskScaleFactor * TaskNumberScale} tasks up to {TaskSize} requests each:");
            Console.WriteLine();
            await Profile("Writer priority system:", new WriterPriorityLockFactory());
            await Profile("Reader priority system:", new ReadPriorityLockFactory());
        }

        static async Task Profile(string introduction, IReadWriteLockFactory lockFactory)
        {
            Console.WriteLine(introduction);
            var globalStart = DateTime.Now;
            var system = new OptimisticExamSystem(HashTableSize, lockFactory);
            var profileResult = await EmulateWork(system, TaskNumberScale);
            var globalEnd = DateTime.Now;
            int total = (int) (globalEnd - globalStart).TotalMilliseconds;
            PrettyPrintAllStats(total, profileResult);
        }

        [NotNull]
        static async Task<ProfileResult> EmulateWork([NotNull] IExamSystem system, int scale)
        {
            int readActionsNumber = 90 * scale;
            int addActionsNumber = 9 * scale;
            int removeActionsNumber = 1 * scale;

            var readActions = RepeatOperation(
                (studentId, courseId) => system.Contains(studentId, courseId),
                readActionsNumber
            );
            var addActions = RepeatOperation(system.Add, addActionsNumber);
            var removeActions = RepeatOperation(system.Remove, removeActionsNumber);

            return new ProfileResult(
                addRequests: await addActions,
                removeRequests: await removeActions,
                containsRequests: await readActions
            );
        }

        [NotNull]
        static async Task<int[]> RepeatOperation(
            [NotNull] Action<long, long> operation,
            int repetitions
        )
        {
            int[] result = new int[repetitions];
            var tasks = new Task[repetitions];
            for (int index = 0; index < repetitions; index += 1)
            {
                tasks[index] = RunOperation(operation, RandomRange());
            }

            for (int index = 0; index < repetitions; index += 1)
            {
                var start = DateTime.Now;
                await tasks[index];
                var end = DateTime.Now;
                result[index] = (int) (end - start).TotalMilliseconds;
            }

            return result;
        }

        static Task RunOperation(Action<long, long> operation, StudentDataRange range)
        {
            return Task.Run(() =>
            {
                for (long studentId = range.FromStudentId; studentId < range.ToStudentId; studentId += 1)
                {
                    for (long courseId = range.FromCourseId; courseId < range.ToCourseId; courseId += 1)
                    {
                        operation(studentId, courseId);
                    }
                }
            });
        }

        static StudentDataRange RandomRange()
        {
            var random = new Random();
            int fromId = random.Next();
            int toId = fromId + random.Next(TaskSize);
            int fromCourse = random.Next();
            int toCourse = fromCourse + random.Next(TaskSize);

            return new StudentDataRange(fromId, toId, fromCourse, toCourse);
        }
    }
}
