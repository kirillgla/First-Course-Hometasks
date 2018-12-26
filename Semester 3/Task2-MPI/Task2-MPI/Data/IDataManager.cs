using JetBrains.Annotations;

namespace Task2_MPI.Data
{
    public interface IDataManager
    {
        [NotNull]
        [ItemNotNull]
        int[][] Provide(int processorsCount);

        void SinkResult([NotNull] [ItemNotNull] int[][] result);
    }
}
