namespace WeakHashTable
{
    public interface ISimpleHashTable<in TKey, TValue>
    {
        void SetValue(TKey key, TValue value);

        bool Remove(TKey key);

        bool ContainsKey(TKey key);

        TValue GetValue(TKey key);

        bool TryGetValue(TKey key, out TValue value);

        TValue this[TKey key] { get; set; }
    }
}
