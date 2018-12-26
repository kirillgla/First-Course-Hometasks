using System;

namespace VectorLength
{
    public class ProcessorAwareAsyncVectorLengthComputer : AsyncVectorLengthComputerBase
    {
        int processorTask;
        
        public ProcessorAwareAsyncVectorLengthComputer(int expectedTaskSize) =>
            processorTask = Math.Min(expectedTaskSize / Environment.ProcessorCount + 1,
                PairwiseAsyncVectorLengthComputer.SmallConstant);

        protected override bool ShouldRunSynchronously(int totalTaskSize) =>
            totalTaskSize < processorTask;
    }
}