using System.Collections.Generic;
using System.Linq;

namespace Tools
{
    public static class Collections
    {
        public static IReadOnlyDictionary<T, U> BuildDictionary<T, U>(IEnumerable<(T, U)> source)
        {
            var result = new Dictionary<T, U>();
            // Oh, don't look at me like that
            // Yes, I don't want to handle too many filters
            foreach (var (key, value) in source.Take(255))
            {
                result.Add(key, value);
            }

            return result;
        }

        public static IEnumerable<int> Range(int min, int max)
        {
            for (int index = min; index <= max; index += 1)
            {
                yield return index;
            }
        }
    }
}
