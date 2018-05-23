using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;
using JetBrains.Annotations;

namespace Task9.Execution
{
    static class Pwd
    {
        [Pure]
        public static object Execute(IEnumerable<object> args, Context context)
        {
            if (args.Any())
            {
                throw new SyntaxErrorException("PWD cannot accept any arguments");
            }

            var result = new StringBuilder();

            result.Append(context.Path);

            Directory.EnumerateDirectories(context.Path)
                .ForEach(dir => result.Append($"{Environment.NewLine}\t<dir> {dir}"));
            Directory.EnumerateFiles(context.Path)
                .ForEach(file => result.Append($"{Environment.NewLine}\t{file}"));

            return result.ToString();
        }
    }
}
