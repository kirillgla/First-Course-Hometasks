using System;
using Core.Synchronization.Factories;
using Core.Synchronization.ReadWrite;
using Core.Utils;
using JetBrains.Annotations;

namespace Core.Collections
{
    public sealed class OptimisticSet : ISimpleSet<CreditData>
    {
        static bool IsValid(CreditData data) => data != CreditData.Min && data != CreditData.Max;

        [NotNull]
        Node Head { get; }

        [TestOnly]
        // Ugly name intentional
        public Node GetHeadSoThatToUseItInTests() => Head;

        [NotNull]
        IReadWriteLockFactory Factory { get; }

        public OptimisticSet([NotNull] IReadWriteLockFactory factory)
        {
            // ReSharper disable once AssignNullToNotNullAttribute - This should not cause problems
            var tail = new Node(CreditData.Max, null, factory);
            Head = new Node(CreditData.Min, tail, factory);
            Factory = factory;
        }

        T Operate<T>(
            [NotNull] Action<IReadWriteLock> locker,
            [NotNull] Action<IReadWriteLock> unlocker,
            [NotNull] Func<Node, Node, T> operation,
            CreditData element
        )
        {
            while (true)
            {
                var (previous, current) = Find(element);
                locker(previous.Lock);
                locker(current.Lock);
                try
                {
                    var (actualPrevious, actualCurrent) = Find(element);
                    if (actualPrevious != previous || actualCurrent != current)
                    {
                        continue;
                    }

                    return operation(previous, current);
                }
                finally
                {
                    unlocker(previous.Lock);
                    unlocker(current.Lock);
                }
            }
        }

        void Operate(
            [NotNull] Action<IReadWriteLock> locker,
            [NotNull] Action<IReadWriteLock> unlocker,
            [NotNull] Action<Node, Node> operation,
            CreditData element
        ) => Operate<object>(locker, unlocker, (previous, current) =>
        {
            operation(previous, current);
            return null;
        }, element);

        public void Add(CreditData element) => Operate(
            ReadWriteUtils.WriteLocker,
            ReadWriteUtils.WriteUnlocker,
            (previous, current) =>
            {
                if (current.Value == element)
                {
                    return;
                }

                var newNode = new Node(element, current, Factory);
                previous.Next = newNode;
            },
            element
        );

        public void Remove(CreditData element) => Operate(
            ReadWriteUtils.WriteLocker,
            ReadWriteUtils.WriteUnlocker,
            (previous, current) =>
            {
                if (current.Value == element)
                {
                    previous.Next = current.Next;
                }
            },
            element
        );

        public bool Contains(CreditData element) => Operate(
            ReadWriteUtils.ReadLocker,
            ReadWriteUtils.ReadUnlocker,
            (_, current) => current.Value == element,
            element
        );

        /// <summary>
        /// Searches for data in list.
        /// </summary>
        /// <returns>first &lt; data; second &gt;= data; first.next = second</returns>
        (Node, Node) Find(CreditData data)
        {
            if (!IsValid(data))
            {
                throw new ArgumentException($"Attempt to insert bad data into optimistic set: {data}");
            }

            var previous = Head;
            var current = previous.Next;
            while (current.Value < data)
            {
                previous = current;
                current = current.Next;
            }

            return (previous, current);
        }

        public sealed class Node
        {
            public CreditData Value { get; }

            [NotNull]
            public IReadWriteLock Lock { get; }

            [NotNull]
            public Node Next { get; set; }

            public Node(CreditData data, [NotNull] Node next, [NotNull] IReadWriteLockFactory factory)
            {
                Value = data;
                Next = next;
                Lock = factory.Create();
            }
        }
    }
}
