using System.Collections.Generic;
using System.Linq;
using Task9.Parsing;

namespace Task9.Tokenization
{
    public sealed class IntegerConstantToken : Token
    {
        public override TokenType TokenType => TokenType.IntegerConstant;
        public override object Value => IntegerConstant;

        public int IntegerConstant { get; }

        public override object Execute(IEnumerable<object> args, Context context, ExecutionType executionType)
        {
            if (args.Any())
            {
                throw new InvalidSyntaxException("Integer constant cannot accepta arguments");
            }

            return IntegerConstant;
        }

        public IntegerConstantToken(int integerConstant)
        {
            IntegerConstant = integerConstant;
        }
    }
}
