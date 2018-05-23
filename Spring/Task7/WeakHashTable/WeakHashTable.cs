using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace WeakHashTable
{
    /// <see cref="KeyValuePair{TKey,TValue}"/>
    public sealed class WeakHashTable<TKey, TValue> : ISimpleHashTable<TKey, TValue>
        where TValue : class
    {
        static int DefaultNumberOfBins => 16;

        public int TimeToStore { get; }

        List<KeyValuePair<TKey, WeakReference<TValue>>>[] Bins { get; }

        static int Hash(TKey key, int top)
        {
            int result = key.GetHashCode() % top;
            return result < 0 ? result + top : result;
        }

        static void Insert(ICollection<KeyValuePair<TKey, WeakReference<TValue>>> list, TKey key, TValue value)
        {
            foreach (var pair in list)
            {
                if (key.Equals(pair.Key))
                {
                    pair.Value.SetTarget(value);
                    return;
                }
            }

            list.Add(new KeyValuePair<TKey, WeakReference<TValue>>(key, new WeakReference<TValue>(value)));
        }

        public WeakHashTable(int timeToStore)
        {
            TimeToStore = timeToStore;
            Bins = new List<KeyValuePair<TKey, WeakReference<TValue>>>[DefaultNumberOfBins];

            for (int i = 0; i < DefaultNumberOfBins; i++)
            {
                Bins[i] = new List<KeyValuePair<TKey, WeakReference<TValue>>>();
            }
        }

        public async void SetValue(TKey key, TValue value)
        {
            int hash = Hash(key, DefaultNumberOfBins);

            Insert(Bins[hash], key, value);

            // TValue value still holds strong reference to target

            await Task.Delay(TimeToStore);

            // Now that's it, argument can be released
        }

        public bool Remove(TKey key)
        {
            int hash = Hash(key, DefaultNumberOfBins);

            KeyValuePair<TKey, WeakReference<TValue>>? target = null;
            bool result = false;

            foreach (var pair in Bins[hash])
            {
                if (key.Equals(pair.Key))
                {
                    target = pair;
                    result = pair.Value.TryGetTarget(out var _);
                    break;
                }
            }

            if (target != null)
            {
                Bins[hash].Remove((KeyValuePair<TKey, WeakReference<TValue>>) target);
            }

            return result;
        }

        public bool ContainsKey(TKey key)
        {
            if (key == null)
            {
                return false;
            }

            int hash = Hash(key, DefaultNumberOfBins);

            foreach (var pair in Bins[hash])
            {
                if (key.Equals(pair.Key))
                {
                    return pair.Value.TryGetTarget(out var _);
                }
            }

            return false;
        }

        public TValue GetValue(TKey key)
        {
            int hash = Hash(key, DefaultNumberOfBins);

            foreach (var pair in Bins[hash])
            {
                if (key.Equals(pair.Key))
                {
                    if (pair.Value.TryGetTarget(out var result))
                    {
                        return result;
                    }

                    throw new KeyNotFoundException();
                }
            }

            throw new KeyNotFoundException();
        }

        public bool TryGetValue(TKey key, out TValue value)
        {
            value = null;
            try
            {
                value = GetValue(key);
                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }

        public TValue this[TKey key]
        {
            get => GetValue(key);
            set => SetValue(key, value);
        }
    }
}
