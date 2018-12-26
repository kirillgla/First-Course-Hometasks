using System;
using System.Collections.Generic;
using JetBrains.Annotations;

namespace Extensions
{
    public static class Enumerables
    {
        public static void ForEach<T>(
            [NotNull] [ItemNotNull] this IEnumerable<T> enumerable,
            [NotNull] Action<T> action
        )
        {
            foreach (var t in enumerable)
            {
                action(t);
            }
        }

        public static void ForEachIndexed<T>(
            [NotNull] [ItemNotNull] this IEnumerable<T> enumerable,
            [NotNull] Action<T, int> action
        )
        {
            int index = 0;
            foreach (var element in enumerable)
            {
                action(element, index);
                index += 1;
            }
        }
    }
}
