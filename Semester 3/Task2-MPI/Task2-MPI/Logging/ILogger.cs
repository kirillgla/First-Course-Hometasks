using JetBrains.Annotations;

namespace Task2_MPI.Logging
{
    public interface ILogger
    {
        void Log([NotNull] string message);

        // Not always true, but avoids quite a lot of errors
        [ContractAnnotation("condition:false => halt")]
        void Assert(bool condition, [NotNull] string message);
    }
}
