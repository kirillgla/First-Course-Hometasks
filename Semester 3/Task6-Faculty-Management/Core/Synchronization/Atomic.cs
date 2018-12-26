using System;
using System.Threading;

namespace Core.Synchronization
{
    public static class Atomic
    {
        /// <summary>
        /// Attempts to replace old value with new one
        /// </summary>
        /// <returns>Whether operation has succeeded</returns>
        public static bool TestAndSet(ref int destination, int newValue, int expectedValue) =>
            Interlocked.CompareExchange(ref destination, newValue, expectedValue) == expectedValue;
    }
}
