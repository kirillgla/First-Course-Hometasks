using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace VectorLength
{
    public abstract class AsyncVectorLengthComputerBase : IVectorLengthComputer
    {
        protected abstract bool ShouldRunSynchronously(int totalTaskSize);

        public double ComputeLength(int[] a)
        {
            var task = ComputeSquaresAsync(a, 0, a.Length);
            task.Wait();
            return Math.Sqrt(task.Result);
        }

        /**
         * Calculates sum of squares of elements in [start; end)
         */
        async Task<long> ComputeSquaresAsync(int[] array, int start, int end)
        {
            if (ShouldRunSynchronously(end - start))
            {
                long result = 0;
                await Task.Run(() => { result = ComputeSquares(array, start, end); });
                return result;
            }

            int center = (end + start) / 2;
            var firstHalf = ComputeSquaresAsync(array, start, center);
            var secondHalf = ComputeSquaresAsync(array, center, end);
            return await firstHalf + await secondHalf;
        }

        static long ComputeSquares(int[] array, int start, int end)
        {
            long result = 0;
            for (int index = start; index < end; index += 1)
            {
                long item = array[index];
                result += item * item;
            }

            return result;
        }
    }
}