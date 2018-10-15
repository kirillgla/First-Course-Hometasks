using System.Collections.Generic;
using JetBrains.Annotations;

namespace Task9.Execution
{
    static class Exit
    {
        [Pure]
        public static object Execute(IEnumerable<object> args)
        {
            throw new ExecutionFinishedException();
        }
    }
}
