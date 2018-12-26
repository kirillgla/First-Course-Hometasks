using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;

namespace Extensions
{
    public static class LinkedLists
    {
        [NotNull]
        public static TSource DropFirst<TSource>([NotNull] [ItemNotNull] this LinkedList<TSource> list)
            where TSource : class
        {
            if (list.Count == 0)
            {
                throw new InvalidOperationException("Attempted to drop first from empty linked list");
            }

            var first = list.First() ?? throw new InvalidOperationException("LinkedList contains forbidden null items");
            list.RemoveFirst();
            return first;
        }
    }
}
