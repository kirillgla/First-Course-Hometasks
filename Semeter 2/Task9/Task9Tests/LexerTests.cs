using System.Collections.Generic;
using Task9.Tokenization;
using Xunit;

namespace Task9Tests
{
    public static class LexerTests
    {
        [Fact]
        public static void TestTokenize()
        {
            var tokens = Lexer.Tokenize("$a=42");
            var correctTokens = new List<Token>
            {
                new VariableToken("a"),
                new OperatorToken(Operator.Assign),
                new IntegerConstantToken(42)
            };

            Assert.Equal(correctTokens, tokens);
        }
        
        [Fact]
        public static void TestGetNextTokenOnCommand()
        {
            const string input = "echo \"Hello, world!\"";

            var token = Lexer.GetNextToken(input, 0);
            Token correctToken = new CommandToken(Command.Echo);
            
            Assert.Equal(correctToken, token);
        }

        [Fact]
        public static void TestGetNextTokenOnConstant()
        {
            const string input = "$a=42";

            var token = Lexer.GetNextToken(input, 3);
            var correctToken = new IntegerConstantToken(42);
            
            Assert.Equal(correctToken, token);
        }
        
        [Fact]
        public static void TestGetNextTokenOnVariable()
        {
            const string input = "$a=42";

            var token = Lexer.GetNextToken(input, 0);
            var correctToken = new VariableToken("a");
            
            Assert.Equal(correctToken, token);
        }
        
        [Fact]
        public static void TestGetNextTokenOnOperator()
        {
            const string input = "$a=42";

            var token = Lexer.GetNextToken(input, 2);
            var correctToken = new OperatorToken(Operator.Assign);
            
            Assert.Equal(correctToken, token);
        }

        [Fact]
        public static void TestParseTokenOnVariable()
        {
            const string tokenString = "$test";

            var token = Lexer.ParseToken(tokenString);
            Token correctToken = new VariableToken("test");
            
            Assert.Equal(correctToken, token);
        }

        [Fact]
        public static void TestParseTokenOnIntegerConstant()
        {
            const string tokenString = "42";

            var token = Lexer.ParseToken(tokenString);
            Token correctToken = new IntegerConstantToken(42);
            
            Assert.Equal(correctToken, token);
        }

        [Fact]
        public static void TestParseTokenOnStringConstant()
        {
            const string tokenString = "\"Hello, world\"";

            var token = Lexer.ParseToken(tokenString);
            Token correctToken = new StringConstantToken("Hello, world");
            
            Assert.Equal(correctToken, token);
        }

        [Fact]
        public static void TestParsTokenOnStringLikeVariable()
        {
            const string tokenString = "\"$var\"";

            var token = Lexer.ParseToken(tokenString);
            Token correctToken = new StringConstantToken("$var");
            
            Assert.Equal(correctToken, token);
        }

        [Theory]
        [InlineData("strangeWord", true)]
        [InlineData("strange_word", true)]
        [InlineData("echo", true)]
        [InlineData("$var", true)]
        [InlineData("=", true)]
        [InlineData("|", true)]
        [InlineData("42", true)]
        [InlineData("var$var", false)]
        [InlineData("var[iable", false)]
        [InlineData("\"Hello, world!", false)]
        [InlineData("\'Hello, world!\"", false)]
        [InlineData("Hello, world!\'", false)]
        [InlineData("\"Hello, world!\"\"", false)]
        [InlineData("\"Hello, world!\"", true)]
        [InlineData("$va\"r", false)]
        [InlineData("'$v a$r'", true)]
        [InlineData("99bottles", false)]
        [InlineData("bottles99", true)]
        [InlineData("Process.Start", true)]
        public static void TestIsValidToken(string tokenString, bool valid)
        {
            bool result = Lexer.IsValidToken(tokenString);
            
            Assert.Equal(valid, result);
        }
        
        [Fact]
        public static void TestGetWordStartOnNoSpaces()
        {
            const string input = "this is sample text";
            
            int wordStart = Lexer.GetWordStart(input, 5);
            
            Assert.Equal(5, wordStart);
        }

        [Fact]
        public static void TestGetWordStartOnSingleSpace()
        {
            const string input = "this is sample text";

            int wordStart = Lexer.GetWordStart(input, 4);
            
            Assert.Equal(5, wordStart);
        }

        [Fact]
        public static void TestGetWordStartOnMultipleSpaces()
        {
            const string input = "this \t is sample text";

            int wordStart = Lexer.GetWordStart(input, 4);
            
            Assert.Equal(7, wordStart);
        }

        [Fact]
        public static void TestGetWordLengthInSimpleText()
        {
            const string input = "this is sample text";

            int wordLength = Lexer.GetWordLength(input, 0);
            
            Assert.Equal(4, wordLength);
        }

        [Fact]
        public static void TestGetWordLengthOnTextWithVariable()
        {
            const string input = "echo $PATH";

            int wordLength = Lexer.GetWordLength(input, 5);
            
            Assert.Equal(5, wordLength);
        }

        [Fact]
        public static void TestGetWordLengthBeforeOperator()
        {
            const string input = "$a=4";

            int wordLength = Lexer.GetWordLength(input, 0);
            
            Assert.Equal(2, wordLength);
        }
        
        [Fact]
        public static void TestGetWordLengthOnOperator()
        {
            const string input = "$a=4";

            int wordLength = Lexer.GetWordLength(input, 2);
            
            Assert.Equal(1, wordLength);
        }

        [Fact]
        public static void TestGetWordLengthOnConstant()
        {
            const string input = "$a=423 ant then stop";

            int wordLength = Lexer.GetWordLength(input, 3);
            
            Assert.Equal(3, wordLength);
        }

        [Fact]
        public static void TestGetWordLengthOnTextNearEnd()
        {
            const string input = "hello, $wor!";

            int wordLength = Lexer.GetWordLength(input, 7);
            
            Assert.Equal(5, wordLength);
        }

        [Fact]
        public static void TestGetWordLengthOnStringConstant()
        {
            const string input = "echo 'Hello, world!'";

            int wordLength = Lexer.GetWordLength(input, 5);
            
            Assert.Equal(15, wordLength);
        }
        
        [Theory]
        [InlineData(' ', true)]
        [InlineData('\t', true)]
        [InlineData('s', false)]
        [InlineData('J', false)]
        [InlineData('$', false)]
        [InlineData("\"", false)]
        public static void TestIsWhitespace(char symbol, bool expected)
        {
            bool symbolIsWhitespace = Lexer.IsWhiteSpace(symbol);
            
            Assert.Equal(expected, symbolIsWhitespace);
        } 
    }
}
