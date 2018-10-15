using System;
using System.IO;
using Task9.Parsing;
using Task9.Tokenization;

namespace Task9
{
    public class CommandHandler
    {
        readonly Context context;

        public CommandHandler() : this(new Context(Directory.GetCurrentDirectory()))
        {
        }

        CommandHandler(Context context)
        {
            this.context = context;
        }

        public void Handle(string command)
        {            
            var tokens = Lexer.Tokenize(command);
            var tree = Parser.Parse(tokens);

            // Console.WriteLine(tree);

            var result = tree.Execute(context);
            Console.WriteLine(result);
        }
    }
}
