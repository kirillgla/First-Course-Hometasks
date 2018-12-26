using JetBrains.Annotations;

namespace Fibers.Collections
{
    public interface IPriorityConverter
    {
        [Pure]
        int Convert(int priority);
    }
}
