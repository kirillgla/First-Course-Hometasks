using JetBrains.Annotations;

namespace Core.Collections
{
    public interface ISimpleSet<in T>
    {
        void Add(T element);
        void Remove(T element);
        bool Contains(T element);
    }
}
