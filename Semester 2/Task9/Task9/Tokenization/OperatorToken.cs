using System.Collections.Generic;
using System.Linq;
using Task9.Parsing;

namespace Task9.Tokenization
{
    public sealed class OperatorToken : Token
    {
        public override TokenType TokenType => TokenType.Operator;
        public override object Value => Operator;
        
        public Operator Operator { get; }

        public override object Execute(IEnumerable<object> args, Context context, ExecutionType executionType)
        {
            var list = args.ToList();
            
            if (list.Count != 2)
            {
                throw new InvalidSyntaxException("Operator was provided incorrect ammount of arguments");
            }

            if (!(list[0] is Variable))
            {
                throw new InvalidSyntaxException("Only variables can be assigned to");
            }

            if (list[1] is Variable)
            {
                list[1] = ((Variable) list[1]).Compute();
            }
            
            ((Variable) list[0]).Set(list[1]);
            
            // This might allow chain assignment: $a=$b=$c=42
            return list[1];
        }

        public OperatorToken(Operator @operator)
        {            
            Operator = @operator;
        }
    }
}
