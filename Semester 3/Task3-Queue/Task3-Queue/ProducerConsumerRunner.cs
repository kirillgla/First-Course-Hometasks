using System;
using System.Threading;
using System.Threading.Tasks;
using JetBrains.Annotations;

namespace Task3_Queue
{
    /// <summary>
    /// It is assumed that no 'run' methods will not be called
    /// after ProducerConsumerQueue is disposed
    /// </summary>
    public sealed class ProducerConsumerRunner : IDisposable
    {
        [NotNull]
        Mutex SharedStateMutex { get; } = new Mutex();

        volatile int activeThreads;

        volatile bool disposed;

        [NotNull]
        IProducerConsumerQueue<string> Queue { get; }

        public ProducerConsumerRunner([NotNull] IProducerConsumerQueue<string> queue) => Queue = queue;

        public async Task RunConsumer(int waitInterval, int producerIndex)
        {
            SharedStateMutex.WaitOne();
            activeThreads += 1;
            SharedStateMutex.ReleaseMutex();

            for (int taskIndex = 0;; taskIndex += 1)
            {
                // ReSharper disable once PossibleNullReferenceException
                await Task.Delay(waitInterval);
                bool shouldClose = false;
                try
                {
                    SharedStateMutex.WaitOne();
                    if (disposed)
                    {
                        // access to the variable is atomic
                        // since lock is already acquired
                        activeThreads -= 1;
                        if (activeThreads == 0)
                        {
                            shouldClose = true;
                        }

                        return;
                    }

                    string task = $"task #{taskIndex} from producer #{producerIndex}";
                    Queue.Add(task);
                }
                finally
                {
                    SharedStateMutex.ReleaseMutex();
                    // current thread is the last one
                    // (= the only one) to access mutex,
                    // so this operation is atomic and safe
                    if (shouldClose)
                    {
                        SharedStateMutex.Dispose();
                    }
                }
            }
        }

        public async Task RunProducer(int waitInterval, int consumerIndex)
        {
            SharedStateMutex.WaitOne();
            activeThreads += 1;
            SharedStateMutex.ReleaseMutex();

            while (true)
            {
                // ReSharper disable once PossibleNullReferenceException
                await Task.Delay(waitInterval);
                bool shouldClose = false;
                try
                {
                    SharedStateMutex.WaitOne();
                    if (disposed)
                    {
                        // access to the variable is atomic
                        // since lock is already acquired
                        activeThreads -= 1;
                        if (activeThreads == 0)
                        {
                            shouldClose = true;
                        }

                        return;
                    }

                    string task = Queue.TryGet();
                    if (task == null)
                    {
                        continue;
                    }

                    Console.WriteLine($"Consumer #{consumerIndex} executes {task}");
                }
                finally
                {
                    SharedStateMutex.ReleaseMutex();
                    // current thread
                    // is the last one (= the only one) to access mutex,
                    // so this operation is atomic
                    if (shouldClose)
                    {
                        SharedStateMutex.Dispose();
                    }
                }
            }
        }

        public void Dispose()
        {
            try
            {
                SharedStateMutex.WaitOne();
                Queue.Dispose();
                disposed = true;
            }
            finally
            {
                // This mutex will be released
                // by last finishing worker thread
                SharedStateMutex.ReleaseMutex();
            }
        }
    }
}
