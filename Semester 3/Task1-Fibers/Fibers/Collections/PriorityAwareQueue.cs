using System.Collections.Generic;
using Extensions;
using JetBrains.Annotations;

namespace Fibers.Collections
{
    public sealed class PriorityAwareQueue<T> where T : class
    {
        int priority;
        public int Priority => IsEmpty ? 0 : priority;

        [NotNull]
        LinkedList<T> Queue { get; }

        public bool IsEmpty => Queue.Count == 0;
        public bool IsNotEmpty => !IsEmpty;

        public PriorityAwareQueue(int priority)
        {
            this.priority = priority;
            Queue = new LinkedList<T>();
        }

        public void Enqueue([NotNull] T value) => Queue.AddLast(value);

        [NotNull]
        public T Dequeue() => Queue.DropFirst();

        public override string ToString() =>
            $"PriorityAwareQueue {{ priority={priority}, size={Queue.Count}}}";
    }
}
