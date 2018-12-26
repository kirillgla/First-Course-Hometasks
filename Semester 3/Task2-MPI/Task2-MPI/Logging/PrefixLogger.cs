using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;

namespace Task2_MPI.Logging
{
    public sealed class PrefixLogger : ILogger
    {
        [NotNull]
        ILogger Delegator { get; }

        [NotNull]
        IList<string> Prefixes { get; }

        [CanBeNull] string prefix;

        [NotNull]
        string Prefix => prefix ?? (prefix = string.Join("", Prefixes));

        public PrefixLogger([NotNull] string prefix, [CanBeNull] ILogger delegator = null)
        {
            switch (delegator)
            {
                case null:
                    Delegator = new ConsoleLogger();
                    Prefixes = new List<string> {prefix};
                    break;
                case PrefixLogger prefixLogger:
                    Delegator = prefixLogger.Delegator;
                    Prefixes = prefixLogger.Prefixes.Concat(prefix).ToList();
                    break;
                default:
                    Delegator = delegator;
                    Prefixes = new List<string> {prefix};
                    break;
            }
        }

        public void Log(string message) => Delegator.Log(Prefix + message);
        public void Assert(bool condition, string message) => Delegator.Assert(condition, Prefix + message);
    }
}
