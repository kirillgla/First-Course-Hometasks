namespace Fibers.Collections
{
    public sealed class ExponentPriorityConverter : IPriorityConverter
    {
        public int Convert(int priority)
        {
            if (priority <= 0)
            {
                return 0;
            }

            return 1 << priority;
        }
    }
}
