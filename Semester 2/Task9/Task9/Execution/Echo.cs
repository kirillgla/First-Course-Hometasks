using System.Collections.Generic;
using System.Linq;
using System.Text;
using JetBrains.Annotations;
using Task9.Tokenization;

namespace Task9.Execution
{
    static class Echo
    {
        [Pure]
        public static object Execute(IEnumerable<object> args)
        {
            return args.Aggregate(new StringBuilder(),
                (builder, arg) =>
                {
                    builder.Append(arg is Variable v ? v.Compute() : arg);
                    builder.Append(" ");
                    return builder;
                },
                builder => builder.ToString());
        }
    }
}
