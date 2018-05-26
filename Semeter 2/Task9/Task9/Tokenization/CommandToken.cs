using System;
using System.Collections.Generic;
using Task9.Execution;
using Task9.Parsing;

namespace Task9.Tokenization
{
    public sealed class CommandToken : Token
    {
        public override TokenType TokenType => TokenType.Command;
        public override object Value => Command;

        public Command Command { get; }

        public override object Execute(IEnumerable<object> args, Context context, ExecutionType executionType)
        {
            switch (Command)
            {
                case Command.Echo:
                    return CommandExecuter.Echo(args);
                case Command.Exit:
                    return CommandExecuter.Exit();
                case Command.Pwd:
                    return CommandExecuter.Pwd(args, context);
                case Command.Cat:
                    return CommandExecuter.Cat(args);
                case Command.Wc:
                    return CommandExecuter.Wc(args, context);
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        public CommandToken(Command command)
        {
            Command = command;
        }
    }
}
