using System;

namespace CustomThreadPool
{
    public static class ThreadPoolUtils
    {
        public const int DefaultThreadCount = 4;

        public static int ProcessorCount
        {
            get
            {
                int count = Environment.ProcessorCount;
                return count > 0 ? count : DefaultThreadCount;
            }
        }
    }
}
