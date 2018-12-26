using System;
using System.IO;
using JetBrains.Annotations;
using MPI;
using Task2_MPI.Data;
using Task2_MPI.Logging;
using Utils;
using Environment = MPI.Environment;

namespace Task2_MPI
{
    static class Program
    {
        // const int DataSize = 4096;
        
        static void Main([NotNull] [ItemNotNull] string[] args)
        {
            if (args.Length != 2)
            {
                PrintUsage();
                return;
            }

            string source = args[0];
            string destination = args[1];

            if (!File.Exists(source))
            {
                Console.WriteLine("Source file does not exist");
                return;
            }
            
            var initialLogger = new PrefixLogger("[Initial] ");
            var infoLogger = new PrefixLogger("[Sorter] ");
            IDataManager manager = new FileDataManager(source, destination);

            string[] environmentArgs = new string[0];
            using (new Environment(ref environmentArgs))
            {
                var world = Communicator.world;
                initialLogger.Assert(world != null, "Could not retrieve world");
                int processors = world.Size;
                initialLogger.Assert(processors.IsPowerOfTwo(), "Number of processors should be power of two");
                int[] batch = GetDataBatch(world, initialLogger, manager, processors);
                var sorter = new Sorter(batch, world, infoLogger);
                int numberOfSteps = world.Size.IntegralBinaryLogarithm();

                for (int step = numberOfSteps - 1; step >= 0; step -= 1)
                {
                    sorter.Exchange(step);
                }

                var gathered = world.Gather(sorter.Sort(), 0);
                if (gathered != null)
                {
                    manager.SinkResult(gathered);
                }
            }
        }

        [NotNull]
        static int[] GetDataBatch(
            [NotNull] Communicator world,
            [NotNull] ILogger logger,
            [NotNull] IDataManager manager,
            int processors
        )
        {
            int[] result = null;
            int rank = world.Rank;
            logger.Log($"Process {rank} decided to get initial data batch");
            if (rank == 0)
            {
                var batches = manager.Provide(processors);
                logger.Log("Process 0 created data batches");
                for (int index = 0; index < processors; index += 1)
                {
                    logger.Log($"Process 0 sends initial data batch to process {index}");
                    var batchToSend = batches[index];
                    if (index == 0)
                    {
                        result = batchToSend;
                    }
                    else
                    {
                        world.Send<int[]>(batchToSend, index, 0);
                    }
                }
            }
            else
            {
                logger.Log($"Process {rank} decided to receive initial batch");
                result = world.Receive<int[]>(0, 0);
            }

            if (result == null)
            {
                throw new InvalidOperationException($"Process {rank} was given null data batch");
            }

            logger.Log($"Process {rank} is done receiving batch. Synchronizing...");
            // This barrier could have been safely removed
            // but is left for debugging purposes
            world.Barrier();
            logger.Log($"Process {rank} finished receiving initial batch");
            return result;
        }

        static void PrintUsage()
        {
            Console.WriteLine("Sorts numbers in parallel.\n" +
                              "Usage: program.exe source destination\n" +
                              "    source: the file containing numbers to be sorted\n" +
                              "    destination: the output file");
        }
    }
}
