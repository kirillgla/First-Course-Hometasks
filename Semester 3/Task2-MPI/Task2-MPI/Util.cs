using System;
using System.Collections.Generic;
using JetBrains.Annotations;

namespace Task2_MPI
{
    public static class Util
    {
        [NotNull]
        [ItemCanBeNull]
        public static IEnumerable<T> Concat<T>(
            [NotNull] [ItemCanBeNull] this IEnumerable<T> first,
            [NotNull] [ItemCanBeNull] IEnumerable<T> second
        )
        {
            using (var firstEnumerator = first.GetEnumerator())
            {
                while (firstEnumerator.MoveNext())
                {
                    yield return firstEnumerator.Current;
                }
            }

            using (var secondEnumerator = second.GetEnumerator())
            {
                while (secondEnumerator.MoveNext())
                {
                    yield return secondEnumerator.Current;
                }
            }
        }


        [NotNull]
        [ItemNotNull]
        public static IEnumerable<T> Concat<T>(
            [NotNull] [ItemNotNull] this IEnumerable<T> source,
            [NotNull] T item
        )
        {
            using (var enumerator = source.GetEnumerator())
            {
                while (enumerator.MoveNext())
                {
                    var current = enumerator.Current;
                    yield return current;
                }

                yield return item;
            }
        }

        [NotNull]
        [ItemCanBeNull]
        public static IEnumerable<TResult> ZipWithNext<T, TResult>(
            [NotNull] this IEnumerable<T> source,
            [NotNull] Func<T, T, TResult> selector
        )
        {
            using (var e = source.GetEnumerator())
            {
                if (!e.MoveNext())
                {
                    yield break;
                }

                var prev = e.Current;
                while (e.MoveNext())
                {
                    yield return selector(prev, e.Current);

                    prev = e.Current;
                }
            }
        }

        [NotNull]
        [ItemNotNull]
        public static T[][] BuildMatrix<T>([NotNull] [ItemNotNull] this T[] source, int rows, int columns)
        {
            if (rows * columns != source.Length)
            {
                throw new ArgumentException();
            }

            T[][] result = new T[rows][];

            for (int index = 0; index < rows; index += 1)
            {
                result[index] = new T[columns];
                for (int jndex = 0; jndex < columns; jndex += 1)
                {
                    result[index][jndex] = source[index * columns + jndex];
                }
            }

            return result;
        }

        [NotNull]
        [ItemNotNull]
        public static IList<T> WithStart<T>([NotNull] [ItemNotNull] this IList<T> source, [NotNull] T item)
        {
            var result = new List<T>();
            result.Add(item);
            result.AddRange(source);
            return result;
        }
    }
}
