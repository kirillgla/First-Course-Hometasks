using System;

namespace VectorLength
{
    public class InstructionParallelVectorLengthComputer : IVectorLengthComputer
    {
        public double ComputeLength(int[] a) => Math.Sqrt(ComputeSquares(a, 0, a.Length));

        /**
         * Sums squares of items in [start; end)
         */
        public static long ComputeSquares(int[] array, int start, int end)
        {
            long evenSum = 0;
            long oddSum = 0;

            int length = end - start;
            int lastLoopIndex = start + GetLastLoopIndex(length);

            for (int index = 0; index < lastLoopIndex; index += 2)
            {
                int arrayI = array[index];
                int arrayIPlusOne = array[index + 1];
                evenSum += arrayI * arrayI;
                oddSum += arrayIPlusOne * arrayIPlusOne;
            }

            if ((length & 1) != 0)
            {
                oddSum += array[end - 1];
            }

            return evenSum + oddSum;
        }

        static int GetLastLoopIndex(int arrayLength) => arrayLength & ~1;
    }
}