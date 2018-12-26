using System;
using JetBrains.Annotations;
using Task2_MPI;
using Utils;
using Xunit;

namespace Task2_MPI_Tests
{
    public sealed class UtilTests
    {
        [Fact]
        public void TestIsPowerOfTwo()
        {
            Assert.False((-8).IsPowerOfTwo());
            Assert.False((-7).IsPowerOfTwo());
            Assert.False((-6).IsPowerOfTwo());
            Assert.False((-5).IsPowerOfTwo());
            Assert.False((-4).IsPowerOfTwo());
            Assert.False((-3).IsPowerOfTwo());
            Assert.False((-2).IsPowerOfTwo());
            Assert.False((-1).IsPowerOfTwo());
            Assert.False(0.IsPowerOfTwo());
            Assert.True(1.IsPowerOfTwo());
            Assert.True(2.IsPowerOfTwo());
            Assert.False(3.IsPowerOfTwo());
            Assert.True(4.IsPowerOfTwo());
            Assert.False(5.IsPowerOfTwo());
            Assert.False(6.IsPowerOfTwo());
            Assert.False(7.IsPowerOfTwo());
            Assert.True(8.IsPowerOfTwo());
            Assert.False(9.IsPowerOfTwo());
            Assert.False(192.IsPowerOfTwo());
            Assert.False(1000.IsPowerOfTwo());
            Assert.True(1024.IsPowerOfTwo());
        }

        [Fact]
        public void TestBinaryLogarithm()
        {
            Assert.Throws<ArgumentException>(() => (-4).IntegralBinaryLogarithm());
            Assert.Throws<ArgumentException>(() => (-3).IntegralBinaryLogarithm());
            Assert.Throws<ArgumentException>(() => (-2).IntegralBinaryLogarithm());
            Assert.Throws<ArgumentException>(() => (-1).IntegralBinaryLogarithm());
            Assert.Throws<ArgumentException>(() => 0.IntegralBinaryLogarithm());
            Assert.Equal(0, 1.IntegralBinaryLogarithm());
            Assert.Equal(1, 2.IntegralBinaryLogarithm());
            Assert.Throws<ArgumentException>(() => 3.IntegralBinaryLogarithm());
            Assert.Equal(2, 4.IntegralBinaryLogarithm());
            Assert.Throws<ArgumentException>(() => 5.IntegralBinaryLogarithm());
            Assert.Throws<ArgumentException>(() => 6.IntegralBinaryLogarithm());
            Assert.Throws<ArgumentException>(() => 7.IntegralBinaryLogarithm());
            Assert.Equal(3, 8.IntegralBinaryLogarithm());
            Assert.Throws<ArgumentException>(() => 9.IntegralBinaryLogarithm());
            Assert.Throws<ArgumentException>(() => 192.IntegralBinaryLogarithm());
            Assert.Equal(10, 1024.IntegralBinaryLogarithm());
        }

        [Fact]
        public void TestBitAtThrows()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => 123.BitAt(-1));
            Assert.Throws<ArgumentOutOfRangeException>(() => 4923.BitAt(-312312));
            Assert.Throws<ArgumentOutOfRangeException>(() => (-12334).BitAt(1321));
            Assert.Throws<ArgumentOutOfRangeException>(() => 0.BitAt(32));
        }

        [Theory]
        [InlineData(0b0001, 0)]
        [InlineData(0b0010, 1)]
        [InlineData(0b0011, 0)]
        [InlineData(0b1000, 3)]
        public void TestBitAtTrue(int number, int index) => Assert.True(number.BitAt(index));

        [Theory]
        [InlineData(0b0000, 2)]
        [InlineData(0b0001, 1)]
        [InlineData(0b0011, 2)]
        [InlineData(0b1011, 2)]
        public void TestBitAtFalse(int number, int index) => Assert.False(number.BitAt(index));

        [Theory]
        [InlineData(-1)]
        [InlineData(-2)]
        [InlineData(-3)]
        [InlineData(-4)]
        [InlineData(-5)]
        [InlineData(-6)]
        [InlineData(-7)]
        [InlineData(-8)]
        public void TestIntegralBinaryExponentThrows(int number) =>
            Assert.Throws<ArgumentOutOfRangeException>(() => number.IntegralBinaryExponent());

        [Theory]
        [InlineData(0, 1)]
        [InlineData(1, 2)]
        [InlineData(3, 8)]
        [InlineData(4, 16)]
        [InlineData(5, 32)]
        [InlineData(6, 64)]
        [InlineData(7, 128)]
        [InlineData(8, 256)]
        [InlineData(9, 512)]
        [InlineData(10, 1024)]
        public void TestIntegralBinaryExponent(int number, int expected) =>
            Assert.Equal(expected, number.IntegralBinaryExponent());

        [Fact]
        void TestBuildSquareMatrix()
        {
            int[] source = {1, 2, 3, 4, 5, 6, 7, 8, 9};
            var matrix = source.BuildMatrix(3, 3);
            int[][] expected = {new[] {1, 2, 3}, new[] {4, 5, 6}, new[] {7, 8, 9}};
            Assert2DArrayEquals(expected, matrix);
        }

        [Fact]
        void TestBuildRectangularMatrix()
        {
            int[] source = {1, 2, 3, 4, 5, 6};
            var matrix = source.BuildMatrix(2, 3);
            int[][] expected = {new[] {1, 2, 3}, new[] {4, 5, 6}};
            Assert2DArrayEquals(expected, matrix);
        }

        [Theory]
        [InlineData(0b0001, 0, 0b0000)]
        [InlineData(0b0010, 0, 0b0010)]
        [InlineData(0b0111, 2, 0b0000)]
        [InlineData(0b1101, 2, 0b1000)]
        void TestTruncateAfter(int source, int index, int expected) =>
            Assert.Equal(expected, source.TruncateAfter(index));

        [Fact]
        void TestUntruncate1()
        {
            const int source = 0b1111;
            const int index = 1;
            int[] expected = { 0b1100, 0b1101, 0b1110, 0b1111 };

            var actual = source.UntruncateAfter(index);
            TestUtils.AssertIterableEquals(expected, actual);
        }

        [Fact]
        void TestUntruncate2()
        {
            const int source = 0b101;
            const int index = 2;
            int[] expected = { 0b000, 0b001, 0b010, 0b011, 0b100, 0b101, 0b110, 0b111 };

            var actual = source.UntruncateAfter(index);
            TestUtils.AssertIterableEquals(expected, actual);
        }

        [Fact]
        void TestUntruncate3()
        {
            const int source = 0b0;
            const int index = 1;
            int[] expected = {0b00, 0b01, 0b10, 0b11};

            var actual = source.UntruncateAfter(index);
            TestUtils.AssertIterableEquals(expected, actual);
        }
        
        void Assert2DArrayEquals<T>([NotNull] [ItemNotNull] T[][] first, [NotNull] [ItemNotNull] T[][] second)
        {
            Assert.Equal(first.Length, second.Length);
            for (int index = 0; index < first.Length; index += 1)
            {
                Assert.Equal(first[index]?.Length, second[index]?.Length);
                for (int jndex = 0; jndex < first[index].Length; jndex += 1)
                {
                    Assert.Equal(first[index][jndex], second[index][jndex]);
                }
            }
        }
    }
}
