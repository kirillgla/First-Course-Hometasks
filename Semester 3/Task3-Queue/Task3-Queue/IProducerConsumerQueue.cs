using System;
using JetBrains.Annotations;

namespace Task3_Queue
{
    public interface IProducerConsumerQueue<T> : IDisposable
    {
        void Add([NotNull] T t);

        [NotNull]
        T Get();

        [CanBeNull]
        T TryGet();
    }
}
