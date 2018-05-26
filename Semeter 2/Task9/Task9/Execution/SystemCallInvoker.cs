using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Task9.Execution
{
    static class SystemCallInvoker
    {
        public static object Execute(string fullName, IEnumerable<object> args)
        {
            string commandLineArguments = args.Aggregate(new StringBuilder(),
                (builder, obj) => builder.Append(obj).Append(" "),
                builder => builder.ToString());

            var process = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    FileName = fullName,
                    Arguments = commandLineArguments,
                    CreateNoWindow = true,
                    RedirectStandardOutput = true,
                    UseShellExecute = false
                }
            };

            process.Start();
            return process.StandardOutput.ReadToEnd();

        }
    }
}
