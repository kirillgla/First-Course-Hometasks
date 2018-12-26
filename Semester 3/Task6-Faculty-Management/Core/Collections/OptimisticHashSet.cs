using System.Collections.Generic;
using Core.Synchronization.Factories;

namespace Core.Collections
{
    public sealed class OptimisticHashSet: ISimpleSet<CreditData>
    {
        IReadOnlyList<ISimpleSet<CreditData>> Bins { get; }

        int BinIndex(CreditData element)
        {
            int result = element.GetHashCode() % Bins.Count;
            return result >= 0 ? result : result + Bins.Count;
        }

        public OptimisticHashSet(int binCount, IReadWriteLockFactory lockFactory)
        {
            var bins = new ISimpleSet<CreditData>[binCount];
            for (int index = 0; index < binCount; index += 1)
            {
                bins[index] = new OptimisticSet(lockFactory);
            }
            
            Bins = bins;
        }

        public void Add(CreditData element) => Bins[BinIndex(element)].Add(element);
        public void Remove(CreditData element) => Bins[BinIndex(element)].Remove(element);
        public bool Contains(CreditData element) => Bins[BinIndex(element)].Contains(element);
    }
}
