using System.Collections.Generic;
using System.Linq;
using Task9.Parsing;

namespace Task9.Tokenization
{
    public sealed class VariableToken : Token
    {
        public override TokenType TokenType => TokenType.Variable;
        public override object Value => Variable;

        public string Variable { get; }

        public override object Execute(IEnumerable<object> args, Context context, ExecutionType executionType)
        {
            if (args.Any())
            {
                throw new InvalidSyntaxException("Variable cannot accept arguments");
            }

            return new Variable(Variable, context);
        }

        public VariableToken(string variable)
        {
            Variable = variable;
        }
    }
}
