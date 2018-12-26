using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using JetBrains.Annotations;

namespace CustomThreadPool
{
    static class Program
    {
        public static void Main()
        {
            // 2 threads are enough to demonstrate
            // that tasks are executed concurrently
            var logger = new ConsoleLogger();
            using (var pool = new ThreadPool.Builder(2).Build(logger))
            {
                Console.WriteLine("Thread pool created");
                CreateTasks(
                        taskCount: 8,
                        taskIterations: 10,
                        delay: 100,
                        logger: logger
                    )
                    .ToList()
                    .ForEach(pool.Enqueue);

                // This delay shows that workers
                // don't stop after executing one task
                Thread.Sleep(2000);
            }

            logger.Log("Thread pool disposed");
            logger.Log("Press any key to continue...");

            // This delay shows that workers
            // do stop once pool is disposed
            Console.ReadKey();
        }

        [Pure]
        [NotNull]
        [ItemNotNull]
        static IEnumerable<Action> CreateTasks(int taskCount, int taskIterations, int delay, [NotNull] ILogger logger)
        {
            for (int workIndex = 0; workIndex < taskCount; workIndex += 1)
            {
                int closureWorkIndex = workIndex;
                yield return () =>
                {
                    for (int workIterationIndex = 0; workIterationIndex < taskIterations; workIterationIndex += 1)
                    {
                        logger.Log($"Doing work {closureWorkIndex}, iteration {workIterationIndex}");
                        Thread.Sleep(delay);
                    }
                };
            }
        }
    }
}
