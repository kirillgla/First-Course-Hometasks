using System;
using System.Linq;

namespace VectorLength
{
    public class SynchronousVectorLengthComputer : IVectorLengthComputer
    {
        public double ComputeLength(int[] a)
        {
            int length = a.Length;
            long result = 0;
            for (int index = 0; index < length; index += 1)
            {
                int element = a[index];
                result += element * element;
            }
            
            return Math.Sqrt(result);
        }
    }
}