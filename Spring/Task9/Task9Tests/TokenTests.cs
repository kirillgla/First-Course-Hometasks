using Task9.Tokenization;
using Xunit;

namespace Task9Tests
{
    public static class TokenTests
    {
        [Fact]
        public static void TestEqualIntegerConstantTokens()
        {
            Token firstToken = new IntegerConstantToken(42);
            Token secondToken = new IntegerConstantToken(42);

            bool equals = firstToken.Equals(secondToken);

            Assert.True(equals);
        }

        [Fact]
        public static void TestNotEqualIntegerConstantTokens()
        {
            Token firstToken = new IntegerConstantToken(42);
            Token secondToken = new IntegerConstantToken(32);

            bool equals = firstToken.Equals(secondToken);
            
            Assert.False(equals);
        }

        [Fact]
        public static void TestEqualStringConstantTokens()
        {
            var firstToken = new StringConstantToken("Hello");
            var secondToken = new StringConstantToken(new string(new[] {'H', 'e', 'l', 'l', 'o'}));

            bool equals = firstToken.Equals(secondToken);

            Assert.True(equals);
        }

        [Fact]
        public static void TestNotEqualStringTokens()
        {
            var firstToken = new StringConstantToken("Hello");
            var secondToken = new StringConstantToken("World");

            bool equals = firstToken.Equals(secondToken);

            Assert.False(equals);
        }

        [Fact]
        public static void TestDifferentTypeTokens()
        {
            Token firstToken = new IntegerConstantToken(42);
            Token secondToken = new StringConstantToken("42");

            bool equals = firstToken.Equals(secondToken);
            bool otherWayEquals = secondToken.Equals(firstToken);
            
            Assert.False(equals);
            Assert.False(otherWayEquals);
        }
    }
}
