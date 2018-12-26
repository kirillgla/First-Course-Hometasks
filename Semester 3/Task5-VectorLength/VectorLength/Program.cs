using System;

namespace VectorLength
{
    static class Program
    {
        const int WarmUpDataSize = 0x10000;
        const int DataSize = 0x10000000;

        static void Main()
        {
            Console.WriteLine("Warming up...");
            WarmUp();
            Console.WriteLine("Creating test data...");
            var data = CreateData(DataSize);
            Console.WriteLine("Testing algorithms...");
            Console.WriteLine();

            // Genuinely synchronous
            Test(new SynchronousVectorLengthComputer(), data, true);

            // Synchronous, but exploits instruction-level parallelism
            Test(new InstructionParallelVectorLengthComputer(), data, true);

            // Splits data into equal halves and sums them independently
            Test(new PairwiseAsyncVectorLengthComputer(), data, true);

            // Doesn't split data to more pieces than number of processors
            Test(new ProcessorAwareAsyncVectorLengthComputer(DataSize), data, true);

            Console.WriteLine();
            Console.WriteLine("Press any key to continue");
            Console.ReadKey();
        }

        static int[] CreateData(int dataSize)
        {
            var result = new int[dataSize];
            var random = new Random();
            for (int index = 0; index < dataSize; index += 1)
            {
                result[index] = random.Next(-100, 100);
            }

            return result;
        }

        static void Test(IVectorLengthComputer sum, int[] data, bool shouldPrint)
        {
            var start = DateTime.Now;
            double answer = sum.ComputeLength(data);
            var end = DateTime.Now;
            string name = sum.GetType().Name;
            if (shouldPrint)
            {
                Console.WriteLine($"{name,-40} returned {answer} in {end - start}");
            }
        }

        static void WarmUp()
        {
            var data = CreateData(WarmUpDataSize);
            Test(new SynchronousVectorLengthComputer(), data, false);
            Test(new InstructionParallelVectorLengthComputer(), data, false);
            Test(new PairwiseAsyncVectorLengthComputer(), data, false);
            Test(new ProcessorAwareAsyncVectorLengthComputer(DataSize), data, false);
        }
    }
}