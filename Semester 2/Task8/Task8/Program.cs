using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Template;

namespace Task8
{
    static class Program
    {
        // Please, provide as first command line argument
        // location of .dll file that contains defenition
        // of type implementing Template.IDoer
        // Example:
        // <project directory location>\TemplateImplementation\bin\Debug\TemplateImplementation.dll
        static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("Please, specify a *.dll address.");
                return;
            }

            if (args.Length > 1)
            {
                Console.WriteLine("Error: too many arguments given.");
                return;
            }

            var doers =
                from type in Assembly.LoadFile(args[0]).ExportedTypes
                where typeof(IDoer).IsAssignableFrom(type)
                from constructor in type.GetTypeInfo().DeclaredConstructors
                where constructor.GetParameters().Length == 0
                select (IDoer) constructor.Invoke(new object[] { });
            
            foreach (var doer in doers)
            {
                try
                {
                    doer.Do();
                }
                catch
                {
                    Console.WriteLine("IDoer throwed an exception.");
                }
            }
        }
    }
}
