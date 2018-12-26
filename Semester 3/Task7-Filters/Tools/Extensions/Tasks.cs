using System.Threading.Tasks;

namespace Tools.Extensions
{
    public static class Tasks
    {
        public static Task Noop { get; } = Task.FromResult<object>(null);
    }
}
