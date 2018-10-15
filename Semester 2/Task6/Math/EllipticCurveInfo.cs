using System.Collections.Generic;
using System.Linq;
using static System.Math;

namespace Math
{
    /// <summary>
    /// Class representing points of Elliptic curve
    /// y^2 = x^3 - x + 1
    /// </summary>
    public sealed class EllipticCurveInfo: CurveInfo
    {
        public override string Name => "Elliptic curve";

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

                yield return new ConnectivityComponent(startWrapper);
            }
        }

        IEnumerable<Point> GetPositivePoints(Region region, double step)
        {
            for (double x = region.UpperLeft.X; x < region.LoweRight.X; x += step)
            {
                double valueUnderRoot = x * x * x - x + 1;

                if (valueUnderRoot < 0)
                {
                    continue;
                }

                double root = Sqrt(valueUnderRoot);
                
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
