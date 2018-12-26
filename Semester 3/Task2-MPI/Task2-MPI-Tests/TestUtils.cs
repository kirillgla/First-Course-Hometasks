using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;

namespace Task2_MPI_Tests
{
    public static class TestUtils
    {
        public static void Assert(this bool condition) => Xunit.Assert.True(condition);

        public static void AssertIterableEquals<T>(
            [CanBeNull] [ItemCanBeNull] IEnumerable<T> first,
            [CanBeNull] [ItemCanBeNull] IEnumerable<T> second
        )
        {
            if (first == null && second == null)
            {
                return;
            }

            Xunit.Assert.NotNull(first);
            Xunit.Assert.NotNull(second);
            
            var firstList = first.ToList();
            var secondList = second.ToList();
            firstList.All(secondList.Contains).Assert();
            secondList.All(firstList.Contains).Assert();
        }
    }
}
