using System;
using System.Collections.Generic;
using System.Linq;

namespace Tools.Extensions
{
    public static class EnumerableExtensions
    {
        public static void ForEach<T>(this IEnumerable<T> data, Action<T> action)
        {
            foreach (var item in data)
            {
                action(item);
            }
        }

        public static void AddSome<T>(this IList<T> list, IEnumerable<T> other, int limit) =>
            list.AddAll(other.Take(limit));

        public static void AddAll<T>(this IList<T> list, IEnumerable<T> other) => other.ForEach(list.Add);
    }
}
