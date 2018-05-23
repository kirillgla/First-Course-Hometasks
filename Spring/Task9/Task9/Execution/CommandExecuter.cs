using System.Collections.Generic;
using JetBrains.Annotations;

namespace Task9.Execution
{
    public static class CommandExecuter
    {
        [Pure]
        public static object Echo(IEnumerable<object> args)
        {
            return Execution.Echo.Execute(args);
        }

        [Pure]
        public static object Exit()
        {
            return Execution.Exit.Execute(null);
        }

        [Pure]
        public static object Pwd(IEnumerable<object> args, Context context)
        {
            return Execution.Pwd.Execute(args, context);
        }

        [Pure]
        public static object Cat(IEnumerable<object> args)
        {
            return Execution.Cat.Execute(args);
        }

        [Pure]
        public static object Wc(IEnumerable<object> args, Context context)
        {
            return Execution.Wc.Execute(args, context);
        }
    }
}
