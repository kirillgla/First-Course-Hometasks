using System;
using System.Linq;
using JetBrains.Annotations;
using MPI;
using Task2_MPI.Logging;
using Utils;

namespace Task2_MPI
{
    sealed class Sorter
    {
        [NotNull]
        ILogger Logger { get; }

        [NotNull]
        Communicator Communicator { get; }

        [NotNull]
        int[] Batch { get; set; }

        public Sorter([NotNull] int[] batch, [NotNull] Communicator communicator, [NotNull] ILogger logger)
        {
            Batch = batch;
            Communicator = communicator;
            Logger = logger;

            new PrefixLogger("[New] ", logger)
                .Log($"Process {Communicator.Rank} created sorter with {Batch.Length} elements.");
        }

        public void Exchange(int step)
        {
            var logger = new PrefixLogger("[Exchange] ", Logger);
            int rank = Communicator.Rank;
            int otherIndex = rank ^ step.IntegralBinaryExponent();
            logger.Log($"Process {rank} decided to performs exchanges for step {step} with process {otherIndex}");
            int[] otherBatch;
            Func<int, bool> condition;

            if (otherIndex > rank)
            {
                Communicator.Send<int[]>(Batch, otherIndex, 0);
                otherBatch = Communicator.Receive<int[]>(otherIndex, 0);
                logger.Assert(otherBatch != null, $"Sorter {rank} received null batch");
                int targetElement = GetTargetElement(step);
                condition = number => number < targetElement;
            }
            else
            {
                otherBatch = Communicator.Receive<int[]>(otherIndex, 0);
                Communicator.Send<int[]>(Batch, otherIndex, 0);
                logger.Assert(otherBatch != null, $"Sorter {rank} received null batch");
                int targetElement = GetTargetElement(step);
                condition = number => number >= targetElement;
            }

            logger.Log($"Process {rank} received a batch of {otherBatch.Length} on step {step}");
            Batch = Batch.Where(condition).Concat(otherBatch.Where(condition)).ToArray();
            logger.Log($"Process {rank} selected {Batch.Length} elements");
            logger.Log($"Process {rank} came to barrier on step {step}");
            Communicator.Barrier();
            logger.Log($"Process {rank} left barrier on step {step}");
        }

        int GetTargetElement(int step)
        {
            var logger = new PrefixLogger("[Target] ", Logger);
            int rank = Communicator.Rank;
            logger.Log($"Process {rank} decided to get target element on step {step}");
            int mainProcessor = rank.TruncateAfter(step);
            int targetElement;
            if (rank != mainProcessor)
            {
                logger.Log($"Process {rank} decided to receive target element from {mainProcessor} on step {step}");
                targetElement = Communicator.Receive<int>(mainProcessor, 0);
                logger.Log($"Process {rank} finished receiving target element from {mainProcessor} on step {step}");
                return targetElement;
            }

            // Too bad NotNullAttribute doesn't seem to work in F#
            // ReSharper disable once AssignNullToNotNullAttribute
            var otherProcessors = rank.UntruncateAfter(step).Where(thatRank => thatRank != rank).ToList();
            logger.Log($"Process {rank} decided to send data to [{string.Join(",", otherProcessors)}] on step {step}");
            targetElement = Batch.First();
            otherProcessors.ForEach(target => Communicator.Send(targetElement, target, 0));
            logger.Log($"Process {rank} finished sending data to other processors on step {step}");
            return targetElement;
        }

        [NotNull]
        public int[] Sort()
        {
            var logger = new PrefixLogger("[Sort] ", Logger);
            logger.Log($"Process {Communicator.Rank} sorts it's batch ({Batch.Length} elements)");
            Array.Sort(Batch);
            return Batch;
        }
        
    }
}
