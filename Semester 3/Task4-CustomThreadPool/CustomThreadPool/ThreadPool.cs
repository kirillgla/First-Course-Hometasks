using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using JetBrains.Annotations;

namespace CustomThreadPool
{
    public sealed class ThreadPool : IDisposable
    {
        ThreadPool()
        {
        }

        [NotNull]
        [ItemNotNull]
        Queue<Action> Tasks { get; } = new Queue<Action>();

        volatile bool running = true;

        [NotNull]
        object ThreadPoolStateLocker { get; } = new object();

        public void Enqueue([NotNull] Action action)
        {
            if (action == null)
            {
                throw new ArgumentNullException(nameof(action));
            }

            if (!running)
            {
                throw new InvalidOperationException("Thread pool has been stopped");
            }

            lock (ThreadPoolStateLocker)
            {
                Tasks.Enqueue(action);
                Monitor.PulseAll(ThreadPoolStateLocker);
            }
        }

        public void Dispose()
        {
            lock (ThreadPoolStateLocker)
            {
                Tasks.Clear();
                running = false;
                Monitor.PulseAll(ThreadPoolStateLocker);
            }
        }

        public sealed class Builder
        {
            int WorkersCount { get; }

            public Builder(int workersCount = 0) => WorkersCount = workersCount;

            [NotNull]
            public ThreadPool Build([NotNull] ILogger logger)
            {
                int workersCount = WorkersCount > 0 ? WorkersCount : ThreadPoolUtils.ProcessorCount;
                var result = new ThreadPool();
                for (int i = 0; i < workersCount; i += 1)
                {
                    CreateWorker(result, logger).Start();
                }

                return result;
            }

            [NotNull]
            static Thread CreateWorker([NotNull] ThreadPool target, [NotNull] ILogger logger) => new Thread(() =>
            {
                logger.Log("Worker thread started");
                while (true)
                {
                    Action task = null;
                    lock (target.ThreadPoolStateLocker)
                    {
                        if (!target.running)
                        {
                            logger.Log("Worker thread died");
                            break;
                        }

                        if (target.Tasks.Any())
                        {
                            task = target.Tasks.Dequeue();
                        }
                        else
                        {
                            logger.Log("Worker thread fell asleep");
                            Monitor.Wait(target.ThreadPoolStateLocker);
                        }
                    }

                    logger.Log($"Worker thread performs action {task}");
                    task?.Invoke();
                }
            });
        }
    }
}
