using System.Collections.Generic;
using System.Text;
using JetBrains.Annotations;

namespace Fibers
{
    public static class Util
    {
        [NotNull]
        public static string JoinToString<T>([ItemNotNull] [NotNull] this IEnumerable<T> enumerable)
        {
            var builder = new StringBuilder("[");
            bool hasPrevious = false;
            foreach (var element in enumerable)
            {
                if (hasPrevious)
                {
                    builder.Append(", ");
                }

                builder.Append(element);
                hasPrevious = true;
            }

            builder.Append("]");
            return builder.ToString();
        }
    }
}
