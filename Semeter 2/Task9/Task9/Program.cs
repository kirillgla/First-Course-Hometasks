using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using Task9.Parsing;
using Task9.Tokenization;

namespace Task9
{
    static class Program
    {
        static void Main()
        {
            var handler = new CommandHandler();
            do
            {
                string input = Console.ReadLine();

                try
                {
                    handler.Handle(input);
                }
                catch (InvalidTokenException exception)
                {
                    Console.WriteLine($"Invalid token: '{exception.Message}'");
                }
                catch (InvalidSyntaxException exception)
                {
                    Console.WriteLine($"Syntax error: {exception.Message}");
                }
                catch (KeyNotFoundException exception)
                {
                    Console.WriteLine($"Read access to unknown variable: '{exception.Message}'");
                }
                catch (ExecutionFinishedException)
                {
                    return;
                }
                catch (NotImplementedException)
                {
                    Console.WriteLine("Not implemented");
                }
            }
            while (true);
        }
    }
}
