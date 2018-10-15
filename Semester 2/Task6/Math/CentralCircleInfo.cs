using System;
using System.Collections.Generic;
using System.Linq;
using static System.Math;

namespace Math
{
    /// <summary>
    /// Class representing points of circle
    /// x^2 + y^2 = Radius^2
    /// </summary>
    public sealed class CentralCircleInfo : CurveInfo
    {
        public override string Name => "Circle";

        double Radius { get; }

        public CentralCircleInfo(double radius)
        {
            if (radius <= 0)
            {
                throw new ArgumentException(
                    $"{nameof(radius)} ({radius}) is not positive and cannot be treated as circle radius");
            }

            Radius = radius;
        }

        internal override IEnumerable<ConnectivityComponent> UnsafeGetConnectivityComponents(Region region, double step)
        {
            if (GetPositivePoints(region, step).Any())
            {
                yield return new ConnectivityComponent(GetPositivePoints(region, step));
            }

            if (GetNegativePoints(region, step).Any())
            {
                yield return new ConnectivityComponent(GetNegativePoints(region, step));
            }

            if (GetPositivePoints(region, step).Any())
            {
                var startWrapper = new List<Point>
                {
                    GetPositivePoints(region, step).First(),
                    GetNegativePoints(region, step).First()
                };
                var endWrapper = new List<Point>
                {
                    GetPositivePoints(region, step).Last(),
                    GetNegativePoints(region, step).Last()
                };
                
                yield return new ConnectivityComponent(startWrapper);
                yield return new ConnectivityComponent(endWrapper);
            }
        }

        

        IEnumerable<Point> GetPositivePoints(Region region, double step)
        {
            double squareRadius = Radius * Radius;

            for (double x = region.UpperLeft.X; x < region.LoweRight.X; x += step)
            {
                double valueUnderRoot = squareRadius - x * x;

                if (valueUnderRoot < 0)
                {
                    continue;
                }

                double root = Sqrt(valueUnderRoot);
                
                // Luckily my shapes are vertically symmetrical
                if (root < region.UpperLeft.Y || root > region.LoweRight.Y)
                {
                    continue;
                }
                
                yield return new Point(x, root);
            }
        }

        IEnumerable<Point> GetNegativePoints(Region region, double step) =>
            from point in GetPositivePoints(region, step)
            select new Point(point.X, -point.Y);
    }
}
