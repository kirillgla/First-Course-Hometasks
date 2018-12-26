using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using JetBrains.Annotations;

namespace Task3_Queue
{
    public sealed class ProducerConsumerQueue : IProducerConsumerQueue<string>
    {
        [NotNull]
        Mutex Mutex { get; } = new Mutex();

        [NotNull]
        [ItemNotNull]
        Queue<string> UnsafeQueue { get; } = new Queue<string>();

        public void Add(string t)
        {
            Mutex.WaitOne();
            try
            {
                UnsafeQueue.Enqueue(t);
            }
            finally
            {
                Mutex.ReleaseMutex();
            }
        }

        public string Get() => TryGet() ?? throw new InvalidOperationException("There are no elements in the queue");

        public string TryGet()
        {
            Mutex.WaitOne();
            try
            {
                return UnsafeQueue.Any() ? UnsafeQueue.Dequeue() : null;
            }
            finally
            {
                Mutex.ReleaseMutex();
            }
        }

        public void Dispose() => Mutex.Dispose();
    }
}
