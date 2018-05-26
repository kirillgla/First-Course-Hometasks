using System.Collections.Generic;
using System.IO;
using Task9.Parsing;

namespace Task9.Execution
{
    static class Cat
    {
        public static object Execute(IEnumerable<object> args)
        {
            string result = null;
            foreach (var arg in args)
            {
                if (result != null)
                {
                    throw new InvalidSyntaxException("cat expected only one argument");
                }

                Stream stream = null;
                StreamReader reader = null;
                try
                {
                    stream = new FileStream(arg.ToString(), FileMode.Open);
                    reader = new StreamReader(stream);
                    result = reader.ReadToEnd();
                }
                catch (IOException)
                {
                    return "Could not access file";
                }
                finally
                {
                    stream?.Dispose();
                    reader?.Dispose();
                }
            }

            if (result != null)
            {
                return result;
            }

            throw new InvalidSyntaxException("cat expected at least one argument");
        }
    }
}
