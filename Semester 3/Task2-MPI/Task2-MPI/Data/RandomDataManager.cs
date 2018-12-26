using System;
using System.Linq;
using JetBrains.Annotations;
using Task2_MPI.Logging;
using Utils;

namespace Task2_MPI.Data
{
    public sealed class RandomDataManager : IDataManager
    {
        [NotNull]
        ILogger Logger { get; }

        int DataSize { get; }

        public RandomDataManager(int dataSize, [NotNull] ILogger logger)
        {
            DataSize = dataSize;
            Logger = logger;
        }

        public int[][] Provide(int processorsCount)
        {
            if (processorsCount <= 0 || DataSize % processorsCount != 0)
            {
                throw new ArgumentException($"Bad processor count: {processorsCount}");
            }

            int batchSize = DataSize / processorsCount;
            return CreateDataBatches(batchSize, processorsCount);
        }

        public void SinkResult(int[][] result)
        {
            var flatResult = result.SelectMany(array => array);
            Logger.Assert(
                flatResult.ZipWithNext((previous, current) => previous <= current).All(element => element),
                "Resulting array is not sorted");
            Logger.Log("Resulting array is sorted!");
        }

        [NotNull]
        [ItemNotNull]
        static int[][] CreateDataBatches(int batchSize, int processors)
        {
            int[][] result = new int[processors][];
            for (int index = 0; index < processors; index += 1)
            {
                result[index] = CreateData(batchSize);
            }

            return result;
        }

        [NotNull]
        static int[] CreateData(int size)
        {
            var random = new Random();
            int[] result = new int[size];
            for (int index = 0; index < size; index += 1)
            {
                result[index] = random.Next();
            }

            return result;
        }

        [NotNull]
        public string PrettyPrintArrayOfArrays([NotNull] [ItemNotNull] int[][] arrayOfArrays)
        {
            var prettyArrays = new string[arrayOfArrays.Length];
            for (int index = 0; index < arrayOfArrays.Length; index++)
            {
                prettyArrays[index] = "[" + string.Join(",", arrayOfArrays[index]) + "]";
            }

            return "[" + string.Join(",\r\n", prettyArrays) + "]";
        }
    }
}
