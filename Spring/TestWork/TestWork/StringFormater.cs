using System;

namespace TestWork
{
    public class StringFormater: IFormater<string>
    {
        public FormatMode Mode { get; }

        public StringFormater(FormatMode mode)
        {
            if (!Enum.IsDefined(typeof(FormatMode), mode))
            {
                throw new ArgumentOutOfRangeException(nameof(mode));
            }

            Mode = mode;
        }
        
        public string Format(string arg)
        {
            return GetPrefixModificator() + arg + GetPostfixModificator();
        }

        protected virtual string GetPrefixModificator()
        {
            switch (Mode)
            {
                case FormatMode.AddTwo:
                    return "++";
                case FormatMode.AddFour:
                    return "++++";
                case FormatMode.AddSix:
                    return "++++++";
                case FormatMode.SubstractSix:
                    return "------";
                case FormatMode.SubstractFour:
                    return "----";
                case FormatMode.SubstractTwo:
                    return "--";
                case FormatMode.None:
                    return string.Empty;
                default:
                    // Never reached actually
                    return null;
            }
        }

        protected virtual string GetPostfixModificator()
        {
            return GetPrefixModificator();
        }
    }
}
