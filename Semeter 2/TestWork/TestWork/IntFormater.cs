using System;

namespace TestWork
{
    public sealed class IntFormater : IFormater<int>
    {
        public FormatMode Mode { get; }

        public IntFormater(FormatMode mode)
        {
            if (!Enum.IsDefined(typeof(FormatMode), mode))
            {
                throw new ArgumentOutOfRangeException(nameof(mode));
            }

            Mode = mode;
        }

        public string Format(int arg)
        {
            return (arg + (int) Mode).ToString();
        }
    }
}
