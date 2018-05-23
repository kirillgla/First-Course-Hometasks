using System.Collections.Generic;
using JetBrains.Annotations;
using Task9.Parsing;

namespace Task9.Tokenization
{
    public abstract class Token
    {
        public abstract TokenType TokenType { get; }

        public abstract object Value { get; }

        [CanBeNull]
        public abstract object Execute(IEnumerable<object> args, Context context, ExecutionType executionType);

        public sealed override bool Equals(object other)
        {
            if (other == null)
            {
                return false;
            }

            if (!(other is Token))
            {
                return false;
            }

            var otherToken = (Token) other;
            
            return TokenType == otherToken.TokenType && Equals(Value, otherToken.Value);
        }

        public sealed override int GetHashCode()
        {
            return unchecked ((int) TokenType * 397) ^ (Value?.GetHashCode() ?? 0);
        }

        public sealed override string ToString()
        {
            return $"{TokenType}: {Value}";
        }
    }
}
