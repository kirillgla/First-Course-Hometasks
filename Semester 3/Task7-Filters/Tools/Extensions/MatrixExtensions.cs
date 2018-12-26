using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;

namespace Tools.Extensions
{
    public static class MatrixExtensions
    {
        [Pure]
        public static double[][] Normalized(this IReadOnlyList<IReadOnlyList<double>> matrix)
        {
            double sum = matrix.Sum(line => line.Sum());
            return (
                from line in matrix
                let newline =
                    from item in line
                    select item / sum
                select newline.ToArray()
            ).ToArray();
        }
    }
}
