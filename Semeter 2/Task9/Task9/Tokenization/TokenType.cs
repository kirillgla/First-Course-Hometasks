namespace Task9.Tokenization
{
    public enum TokenType
    {
                         // Examples:
        Command,         // "echo", "exit"
        IntegerConstant, // "4"
        StringConstant,  // "'Hello, world!'"
        Variable,        // "$myCustomVariable", "$a"
        Operator,        // "=", "|"
        Unknown          // "Start"
    }
}
