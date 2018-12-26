namespace VectorLength
{
    public class PairwiseAsyncVectorLengthComputer : AsyncVectorLengthComputerBase
    {
        public const int SmallConstant = 100;
        protected override bool ShouldRunSynchronously(int totalTaskSize) =>
            totalTaskSize < SmallConstant;
    }
}
