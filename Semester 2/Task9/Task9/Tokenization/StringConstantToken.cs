using System.Collections.Generic;
using System.Linq;
using Task9.Parsing;

namespace Task9.Tokenization
{
    /// Not to be confused with [mscorlib]System.Reflection.Emit.StringToken
    public sealed class StringConstantToken : Token
    {
        public override TokenType TokenType => TokenType.StringConstant;
        public override object Value => StringConstant;

        public string StringConstant { get; }

        public override object Execute(IEnumerable<object> args, Context context, ExecutionType executionType)
        {
            return StringConstant;
        }

        public StringConstantToken(string stringConstant)
        {
            StringConstant = stringConstant;
        }
    }
}
