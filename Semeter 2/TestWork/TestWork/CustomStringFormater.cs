namespace TestWork
{
    public delegate string GetModificator(FormatMode mode);

    public sealed class CustomStringFormater : StringFormater
    {
        GetModificator GetModificator { get; }

        public CustomStringFormater(FormatMode mode, GetModificator getModificator) : base(mode)
        {
            GetModificator = getModificator;
        }

        protected override string GetPrefixModificator()
        {
            return GetModificator(Mode);
        }

        protected override string GetPostfixModificator()
        {
            return GetModificator((FormatMode) (-(int) Mode));
        }
    }
}
