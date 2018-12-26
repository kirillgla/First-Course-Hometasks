using Tools;
using Tools.Extensions;
using Xunit;

namespace Tests
{
    public sealed class ExtensionsTests
    {
        [Theory]
        [InlineData(1, true)]
        [InlineData(2, false)]
        [InlineData(3, false)]
        [InlineData(4, true)]
        [InlineData(5, false)]
        [InlineData(6, false)]
        [InlineData(7, false)]
        [InlineData(8, false)]
        [InlineData(9, true)]
        [InlineData(10, false)]
        public void TestIsPerfectSquare(int number, bool square) => Assert.Equal(square, number.IsPerfectSquare());
    }
}
