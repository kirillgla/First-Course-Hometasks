using System.Collections.Generic;
using System.Linq;
using Task9.Execution;
using Task9.Parsing;

namespace Task9.Tokenization
{
    public sealed class UnknownToken : Token
    {
        public override TokenType TokenType => TokenType.Unknown;
        public override object Value => Representation;

        public string Representation { get; }

        public override object Execute(IEnumerable<object> args, Context context, ExecutionType executionType) =>
            SystemCallInvoker.Execute(Representation, args.ToArray());

        public UnknownToken(string representation)
        {
            Representation = representation;
        }
    }
}
