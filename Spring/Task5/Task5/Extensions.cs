using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace Task5
{
    public static class Extensions
    {
        public static void ForEach<T>(this IEnumerable<T> collection, Action<T> action)
        {
            if (collection is null)
            {
                throw new NullReferenceException(nameof(collection));
            }

            if (action is null)
            {
                throw new NullReferenceException(nameof(action));
            }
            
            foreach (var item in collection)
            {
                action(item);
            }
        }

        public static void Ignore(this Task task)
        {
            // Because why not to check?
            if (task is null)
            {
                throw new NullReferenceException(nameof(task));
            }
        }

        public static void Ignore<T>(this Task<T> task)
        {
            if (task is null)
            {
                throw new NullReferenceException(nameof(Task));
            }
        }
    }
}
