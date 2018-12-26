using System;
using System.IO;
using System.Linq;
using JetBrains.Annotations;

namespace Task2_MPI.Data
{
    public sealed class FileDataManager : IDataManager
    {
        [NotNull]
        string Input { get; }

        [NotNull]
        string Output { get; }

        public FileDataManager([NotNull] string input, [NotNull] string output)
        {
            Input = input;
            Output = output;
        }

        public int[][] Provide(int processorsCount)
        {
            int[] input = ReadInts();
            int dataSize = input.Length;
            if (dataSize % processorsCount != 0)
            {
                throw new ArgumentException("Data cannot be divided equally between processors");
            }

            int batchSize = dataSize / processorsCount;

            return input.BuildMatrix(processorsCount, batchSize);
        }

        [NotNull]
        int[] ReadInts() => File.ReadAllText(Input).Split(' ').Select(int.Parse).ToArray();

        public void SinkResult(int[][] result)
        {
            string answer = string.Join(" ", result.Select(array => string.Join(" ", array)));
            File.WriteAllText(Output, answer);
        }
    }
}
