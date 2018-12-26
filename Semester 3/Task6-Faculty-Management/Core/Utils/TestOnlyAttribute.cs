using System;

namespace Core.Utils
{
    /// <summary>
    /// Indicates that member should only be used in tests 
    /// </summary>
    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Property | AttributeTargets.Constructor)]
    public sealed class TestOnlyAttribute : Attribute
    {
    }
}
